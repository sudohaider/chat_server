-module(chat_server).
-export([start/0, accept/1]).

start() ->
    {ok, ListenSocket} = gen_tcp:listen(1234, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]),
    spawn(fun() -> accept(ListenSocket) end),
    io:format("Chat server started.~n").

accept(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(fun() -> accept(ListenSocket) end),
    handle_client(Socket).

handle_client(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Name} ->
            io:format("Client ~p connected~n", [Name]),
            handle_client(Socket);
        {error, closed} ->
            ok
    end.
