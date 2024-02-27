%% File: src/chat_server.erl
-module(chat_server).
-export([start/0, stop/0]).

start() ->
    %% Your code to start the chat server goes here
    io:format("Chat server started.~n").

stop() -> 
    io:format("Chat server stopped.~n").

