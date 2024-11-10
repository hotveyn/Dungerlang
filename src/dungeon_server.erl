%%%====================================================================
%%% @author Ioann Shestopalov <ioann.shestopalov@gmail.com>
%%%  [https://github.com/hotveyn]
%%% @doc RPC over TCP server. This module defines a server process that
%%%      listens for incoming TCP connections and allows the user to
%%%      execute RPC commands via that TCP stream.
%%% @end
%%%====================================================================

-module(dungeon_server).
-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_server).

%% API
-export([start_link/2, start_link/1, get_count/1, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(DEFAULT_PORT, 1055).

-record(state, {lsock, request_count = 0}).

%%%====================================================================
%%% API
%%%====================================================================


%% @doc Start the server.
-spec start_link(string(), integer()) -> {ok, pid()}.
start_link(Server, Port) ->
    gen_server:start_link({local, Server}, ?MODULE, [Port], []).

%% @doc Calls "start_link(Server, Port)." using the default port
-spec start_link(string()) -> {ok, pid()}.
start_link(Server) ->
    start_link(Server, ?DEFAULT_PORT).

%% @doc Return numbers of requests.
-spec get_count(string()) -> {ok, integer()}.
get_count(Server) ->
    io:format("get_count"),
    gen_server:call(Server, get_count).

%% @doc Stop the server.
-spec stop(string()) -> {ok}.
stop(Server) ->
    gen_server:cast(Server, stop).

%%%====================================================================
%%% gen_server callbacks
%%%====================================================================

init([Port]) ->
    {ok, LSock} = gen_tcp:listen(Port, [{active, true}]),
    {ok, #state{lsock = LSock}, 0}.

handle_call(get_count, _From, #state{request_count = RequestCount} = State) ->
    io:format("handle_call get_count"),
    {reply, {ok, RequestCount}, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({tcp, Socket, RawData}, #state{request_count = RequestCount} = State) -> 
    io:format("handle_info tcp"),
    do_rpc(Socket, RawData),
    {noreply, State#state{request_count = RequestCount + 1}};

handle_info(timeout, #state{lsock = LSock} = State) ->
    io:format("handle_info timeout"),
    {ok, _Sock} = gen_tcp:accept(LSock),
    io:format("handle_info timeout gen_tcp:accept"),
    {noreply, State}.

terminate(_Reasone, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%====================================================================
%%% Internal functions
%%%====================================================================

do_rpc(Socket, RawData) ->
    try
        io:format("do_rpc: RawData = ~p~n", [RawData]),
        {M, F, A} = split_out_mfa(RawData),
        io:format("do_rpc: split_out_mfa(RawData)->[M,F,A] = ~p~n", [[M,F,A]]),
        Result = apply(M,F,A),
        io:format("do_rpc: apply(M,F,A)->Result = ~p~n", [Result]),
        gen_tcp:send(Socket, io_lib:fwrite("~p~n", [Result]))
    catch
        _Class:Err -> 
            gen_tcp:send(Socket, io_lib:fwrite("~p~n", [Err]))
    end.

split_out_mfa(RawData) ->
    MFA = re:replace(RawData, "\r\n$", "", [{return, list}]),
    io:format("split_out_mfa: re:replace(RawData, ...) -> MFA = ~p~n", [MFA]),
    {match, [M, F, A]} = re:run(MFA, "^(.*):(.*)\\((.*)\\)$", [{capture, [1,2,3], list}, ungreedy]),
    {list_to_atom(M), list_to_atom(F), args_to_terms(A)}.

args_to_terms(RawArgs) ->
    io:format("args_to_terms: RawArgs = ~p~n", [RawArgs]),
    {ok, Toks, _Line} = erl_scan:string("[" ++ RawArgs ++ "]. ", 1),
    {ok, Args} = erl_parse:parse_term(Toks),
    Args.

%%%====================================================================
%%% Unit tests
%%%====================================================================

start_test() ->
    {ok, _} = start_link(test, 7777).

count_test() ->
    {ok, _} = start_link(aboba, 7778),
    os:cmd("echo \"io:forma(\"\")\" | telnet localhost 7778"),
    {count, 1} = get_count(aboba).