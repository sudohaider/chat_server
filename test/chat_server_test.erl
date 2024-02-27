-module(chat_server_test).
-include_lib("eunit/include/eunit.hrl").

start_chat_server_test() ->
    chat_server:start().

stop_chat_server_test() ->
    chat_server:stop().

