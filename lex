#!/usr/bin/env escript
%% -*- erlang -*-
%%%-------------------------------------------------------------------
%%% @author  falood <falood@gmail.com>
%%% @copyright (C) 2012, 
%%% @doc
%%% 编译器 词法分析程序 生成Token文件
%%% @end
%%% Created : 20 Sep 2012 by  falood <falood@gmail.com>
%%%-------------------------------------------------------------------
usage() ->
    S = "本程序为CMM语言词法分析程序，运行程序使用\nescript lex infile [-f outfile]
",
    io:format("~ts~n", [S]),
    halt(1).

main([]) ->
    usage();
main([IN_FILE|T]) ->
    case fileread(IN_FILE) of
        {ok, S} ->
            start_db(),
            lexical(S),
            stdwrite(),
            case T of
                ["-f", OUT_FILE] ->
                    filewrite(OUT_FILE);
                _ ->
                    donothing
            end;
        {error, _} ->
            io:format("~p: No such file or directory~n", [IN_FILE])
    end,
    init:stop().

fileread(FileName) ->
    case file:read_file(FileName) of
        {ok, B} ->
            {ok, binary_to_list(B)};
        Error ->
            Error
    end.

filewrite(OUT_FILE) ->
    db ! {get, self()},
    receive
        {Tokens, _Errors, _Ignores} ->
            file:write_file(OUT_FILE, []), % 建立、清空输出文件
            lists:map(fun(X) ->
                              S = io_lib:format("~p.~n", [X]),
                              file:write_file(OUT_FILE, S, [append])
                      end, Tokens);
        Any ->
            io:format("RPC ERROR! GET ~p~n", [Any])
    end.

stdwrite() ->
    db ! {get, self()},
    receive
        {Tokens, Errors, Ignores} ->
            LT = lists:flatlength(Tokens),
            LE = lists:flatlength(Errors),
            LI = lists:flatlength(Ignores),
            io:format("Success ~p, Errors ~p, Ignores ~p~n", [LT, LE, LI]),
            io:format("Success ~p: ~n", [LT]),
            lists:map(fun({Type, String, Line}) ->
                              io:format(
                                "token type ~p, token value is ~p, line number is ~p~n", 
                                [Type, String, Line])
                      end, Tokens),
            io:format("Ignore ~p: ~n", [LI]),
            lists:map(fun({Char, Line}) ->
                              io:format("line ~p: ignore char ~c Error~n", [Line, Char])
                      end, Ignores),
            io:format("Error ~p: ~n", [LE]),
            lists:map(fun({String, Line}) ->
                              io:format("line ~p: ~p Error~n", [Line, String])
                      end, Errors),
            io:format("DONE~n");
        Any ->
            io:format("RPC ERROR! GET ~p~n", [Any])
    end.

%% commit 为处理函数，当 prelex 匹配到词之后会调用 commit 处理
commit({ok, _, _, _Line}=Z) ->
    db ! Z;
commit({error, _, _Line}=Z) ->
    db ! Z;
commit({ignore, _, _Line}=Z) ->
    db ! Z;

commit({found, _Type, [], _LineNum}) ->
    pass;
commit({found, real, CurrentS, LineNum}) ->
    case re:run(CurrentS, "^[0-9]+\.[0-9]+$") of
        nomatch ->
            commit({error, "REAL", LineNum});
        _ ->
            S = lists:reverse(CurrentS),
            commit({ok, real, S, LineNum})
    end;
commit({found, identify, CurrentS, LineNum}) ->
    S = lists:reverse(CurrentS),
    case re:run(S, "^[a-zA-Z]([a-zA-Z0-9_]*[a-zA-Z0-9])*$") of
        nomatch ->
            commit({error, "IDENTIFY", LineNum});
        _ ->
            case lists:member(S, ["if", "else", "while", "read", "write", "int", "real"]) of
                true ->
                    commit({ok, keyword, S, LineNum});
                false ->
                    commit({ok, identify, S, LineNum})
            end
    end;
commit({found, operator, "=", LineNum}) ->
    commit({ok, operator, "=", LineNum});
commit({found, operator, CurrentS, LineNum}) ->
    S = lists:reverse(CurrentS),
    case lists:member( S, ["==", "<", ">", "<>", ">", "<=", ">="]) of
        true ->
            commit({ok, comparation, S, LineNum});
        false ->
            case lists:member(S, ["+", "-", "*", "/"]) of
                true ->
                    commit({ok, computing, S, LineNum});
                false ->
                    commit({error, S, LineNum})
            end
        end;
commit({found, Type, CurrentS, LineNum}) ->
    S = lists:reverse(CurrentS),
    commit({ok, Type, S, LineNum}),
    ok.

%% prelex/4 为词法分析主函数，逐字分析，返回 Token 串
lexical(S) ->
    lexical(none, [], 1, S).

lexical(_Type, _CurrentS, _LineNum, [])->
    done;
%% 单行注释
lexical(commentoneline, _CurrentS, LineNum, [$\n|T])->
    lexical(none, [], LineNum + 1, T);
lexical(commentoneline, _CurrentS, LineNum, [_H|T])->
    lexical(commentoneline, [], LineNum, T);

%% 多行注释
lexical(comment, [$*|_CurrentS], LineNum, [$/|T]) ->
    lexical(none, [], LineNum, T);
lexical(comment, CurrentS, LineNum, [$\n|T]) ->
    lexical(comment, CurrentS, LineNum + 1, T);
lexical(comment, CurrentS, LineNum, [H|T]) ->
    lexical(comment, [H|CurrentS], LineNum, T);

%% 换行符
lexical(Type, CurrentS, LineNum, [$\n|T])->
    commit({found, Type, CurrentS, LineNum}),
    lexical(Type, CurrentS, LineNum + 1, T);

%% 空白符
lexical(Type, CurrentS, LineNum, [H|T]) when H == $\t;
                                            H == $ ;
                                            H == $\r ->
    commit({found, Type, CurrentS, LineNum}),
    lexical(none, [], LineNum, T);
%% 圆括号
lexical(Type, CurrentS, LineNum, [H|T]) when H == $(; H == $) ->
    commit({found, Type, CurrentS, LineNum}),
    commit({found, parenthesis, [H], LineNum}),
    lexical(none, [], LineNum, T);
%% 分隔符
lexical(Type, CurrentS, LineNum, [H|T]) when H == $;;
                                            H == ${; H == $} ->
    commit({found, Type, CurrentS, LineNum}),
    commit({found, separation, [H], LineNum}),
    lexical(none, [], LineNum, T);
%% 数字
lexical(none, _CurrentS, LineNum, [H|T]) when H >= $0, H =< $9 ->
    lexical(integer, [H], LineNum, T);
lexical(integer, CurrentS, LineNum, [H|T]) when H >= $0, H =< $9 ->
    lexical(integer, [H|CurrentS], LineNum, T);
lexical(real, CurrentS, LineNum, [H|T]) when H >= $0, H =< $9 ->
    lexical(real, [H|CurrentS], LineNum, T);
lexical(identify, CurrentS, LineNum, [H|T]) when H >= $0, H =< $9 ->
    lexical(identify, [H|CurrentS], LineNum, T);
lexical(Type, CurrentS, LineNum, [H|T]) when H >= $0, H =< $9 ->
    commit({found, Type, CurrentS, LineNum}),
    lexical(integer, [H], LineNum, T);

%% 小数点
lexical(integer, CurrentS, LineNum, [$.|T]) ->
    lexical(real, [$.|CurrentS], LineNum, T);
lexical(Type, CurrentS, LineNum, [$.|T]) ->
    commit({error, ".", LineNum}),
    lexical(Type, CurrentS, LineNum, T);

%% 字母和下划线
lexical(none, _CurrentS, LineNum, [H|T]) when H >= $a, H =< $z;
                                             H >= $A, H =< $Z; H == $_ ->
    lexical(identify, [H], LineNum, T);
lexical(identify, CurrentS, LineNum, [H|T]) when H >= $a, H =< $z;
                                                H >= $A, H =< $Z; H == $_ ->
    lexical(identify, [H|CurrentS], LineNum, T);
lexical(Type, CurrentS, LineNum, [H|T]) when H >= $a, H =< $z;
                                            H >= $A, H =< $Z; H == $_ ->
    commit({found, Type, CurrentS, LineNum}),
    lexical(identify, [H], LineNum, T);

%% / 号开始单行注释
lexical(Type, [$/|CurrentS], LineNum, [$/|H]) ->
    commit({found, Type, CurrentS, LineNum}),
    lexical(commentoneline, "//", LineNum, H);

%% /* 号开始多行注释
lexical(Type, [$/|CurrentS], LineNum, [$*|H]) ->
    commit({found, Type, CurrentS, LineNum}),
    lexical(comment, "/*", LineNum, H);

%% + - * / = < > 号
lexical(none, _CurrentS, LineNum, [H|T]) when H == $+; H == $-;
                                             H == $*; H == $/;
                                             H == $<; H == $>; H == $= ->
    lexical(operator, [H], LineNum, T);
lexical(operator, CurrentS, LineNum, [H|T]) when H == $+; H == $-;
                                                H == $*; H == $/;
                                                H == $<; H == $>; H == $= ->
    lexical(operator, [H|CurrentS], LineNum, T);
lexical(Type, CurrentS, LineNum, [H|T]) when H == $+; H == $-;
                                            H == $*; H == $/;
                                            H == $<; H == $>; H == $= ->
    commit({found, Type, CurrentS, LineNum}),
    lexical(operator, [H], LineNum, T);

%% [ ] 号
lexical(Type, CurrentS, LineNum, [$[|H]) ->
    commit({found, Type, CurrentS, LineNum}),
    lexical(arraystart, [$[], LineNum, H);
lexical(Type, CurrentS, LineNum, [$]|H]) ->
    commit({found, Type, CurrentS, LineNum}),
    lexical(arrayend, [$]], LineNum, H);

%% 其它字符认为是非法字符，忽略
lexical(Type, CurrentS, LineNum, [H|T])->
    commit({ignore, H, LineNum}),
    lexical(Type, CurrentS, LineNum, T).
    
start_db() ->
    Pid = spawn(fun() ->loop({[], [], []}) end),
    register(db, Pid).

loop({Tokens, Errors, Ignores}) ->
    receive
        {ok, Type, String, Line} ->
            loop({[{Type, String, Line}|Tokens], Errors, Ignores});
        {error, String, Line} ->
            loop({Tokens, [{String, Line}|Errors], Ignores});
        {ignore, Char, Line} ->
            loop({Tokens, Errors, [{Char, Line}|Ignores]});
        {get, From} ->
            From ! {lists:reverse(Tokens), lists:reverse(Errors), lists:reverse(Ignores)},
            loop({Tokens, Errors, Ignores});
        stop ->
            stop;
        _ ->
            loop({Tokens, Errors, Ignores})
    end.
