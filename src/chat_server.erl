-module(chat_server).
-export([start/0, accept/2, handle_client/2]).

-record(room, {name, creator, members = []}).
-record(state, {rooms = [], clients = []}).

start() ->
    io:format("START"),
    State = ets:new(state, [public, named_table]),
    ets:insert(State, {rooms, []}),
    ets:insert(State, {clients, []}),
    {ok, ListenSocket} = gen_tcp:listen(1234, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]),
    spawn(fun() -> accept(ListenSocket, State) end).

accept(ListenSocket, State) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),

    {ok, {RemoteIP, RemotePort}} = inet:peername(Socket),

    io:format("ACCEPT ~p ~p ~p \n", [Socket, RemotePort, RemoteIP]),
    % {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(fun() -> accept(ListenSocket, State) end),
    handle_client(Socket, State).

handle_client(Socket, State) ->
    io:format("HANDLE\n"),
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            Command = binary_to_list(Data),
            StrippedCommand = re:replace(Command, "\r|\n", "", [global, {return, list}]),
            NewState = process_command(StrippedCommand, Socket, State),
            io:format("NEW STATE: ~p\n", [NewState]),
            handle_client(Socket, NewState);
        {error, closed} ->
            ok
    end.

process_command(Command, Socket, State) ->
    io:format("COMMAND: ~p\n ~p", [Command, string:tokens(Command, " ")]),
    case string:tokens(Command, " ") of
        ["CREATE", RoomName] ->
            create_room(RoomName, Socket, State);
        ["DESTROY", RoomName] ->
            destroy_room(RoomName, Socket, State);
        ["LIST"] ->
            NewState = list_rooms(Socket, State),
            NewState;
        ["JOIN", RoomName] ->
            join_room(RoomName, Socket, State);
        ["LEAVE", RoomName] ->
            leave_room(RoomName, Socket, State);
        ["SEND", RoomName, Message] ->
            send_message(RoomName, Message, Socket, State);
        ["SENDPRIVATE", RoomName, UserName, Message] -> 
            send_message_to_user(UserName, RoomName, Message, Socket, State);
        ["LISTMEMBERS", RoomName] -> 
            list_members(RoomName, State, Socket);
        _ ->
            State
    end.

create_room(RoomName, Socket, State) ->
    {ok, {RemoteIP, RemotePort}} = inet:peername(Socket),

    Room = #room{name = RoomName, creator = Socket},
    OldRooms = case ets:lookup(State, rooms) of
        [] -> [];
        [{rooms, R}] -> R
    end,
    io:format("Newly created Room: ~p\n", [Room]),
    NewRooms = [Room | OldRooms],
    ets:insert(State, {rooms, NewRooms}), 
    State.

destroy_room(RoomName, Socket, State) ->
    {NewRooms, Room} = lists:partition(fun(#room{name = N, creator = C}) -> N /= RoomName orelse C /= Socket end,  ets:lookup_element(state, rooms, 2)),
    case Room of
        [] -> ok;  %% or however you want to handle this case
        _ -> 
            lists:foreach(fun(Client) -> gen_tcp:send(Client, "Room destroyed\n") end, (hd(Room))#room.members),
            ets:insert(State, {rooms, NewRooms})
    end,
    State.

list_rooms(Socket, State) ->
    io:format("LIST-STATE: ~p\n", [State]),
    [{rooms, Rooms}] = ets:lookup(State, rooms),
    io:format("LIST-ROOMS: ~p\n", [Rooms]),
    RoomNames = lists:map(fun(#room{name = N}) -> N end, Rooms),
    gen_tcp:send(Socket, io_lib:format("all rooms:~p\n", [RoomNames])),
    State.

join_room(RoomName, Socket, State) ->
    [{rooms, OldRooms}] = ets:lookup(State, rooms),
    io:format("OLD-ROOMS: ~p\n", [OldRooms]),
    case lists:partition(fun(#room{name = N}) -> N == RoomName end, OldRooms) of
        {[], _OtherRooms} ->
            %% No room with the given name was found, handle this case appropriately
            State;
        {[Room], OtherRooms} ->
            {ok, {RemoteIP, RemotePort}} = inet:peername(Socket),
            io:format("peername: ~p", [RemotePort]),
            NewRoom = Room#room{members = [Socket | Room#room.members]},
            NewRooms = [NewRoom | OtherRooms],
            io:format("NEW-ROOMS: ~p\n", [NewRoom]),
            io:format("OLD-ROOMS: ~p\n", [NewRoom]),


            ets:insert(State, {rooms, NewRooms}),
            State
    end.

leave_room(RoomName, Socket, State) ->
    {Room, OtherRooms} = lists:partition(fun(#room{name = N}) -> N == RoomName end, ets:lookup_element(state, rooms, 2)),
    NewRoom = (hd(Room))#room{members = lists:delete(Socket, (hd(Room))#room.members)},
    ets:insert(State, {rooms, [NewRoom | OtherRooms]}).

send_message(RoomName, Message, Socket, State) ->
    {Room, X} = lists:partition(fun(#room{name = N}) -> N == RoomName end, ets:lookup_element(state, rooms, 2)),
    io:format("SEND-ROOM: ~p\n", [(hd(Room))#room.members]),
    io:format("SEND-rooms: ~p\n", [X]),
    lists:foreach(fun(Client) -> gen_tcp:send(Client, io_lib:format("~p: ~s\n", [Socket, Message])) end, (hd(Room))#room.members),
    State.

send_message_to_user(Username, RoomName, Message, Socket, State) ->
    {Room, _} = lists:partition(fun(#room{name = N}) -> N == RoomName end, ets:lookup_element(state, rooms, 2)),
    io:format("UserName ~p", [Username]),
    io:format("Room: ~p", [(hd(Room))#room.members] ),
    UserSocket = find_socket_by_port(Username, Room),
    io:format("UserSocket: ~p\n", [UserSocket]),
    case UserSocket of
        false ->
            io:format("User not found in the room\n"),
            State;
        _ ->
            gen_tcp:send(hd(UserSocket), io_lib:format("~p: ~s\n", [Socket, Message])),
            State
    end.

list_members(RoomName, State, Socket) ->
    {Room, _} = lists:partition(fun(#room{name = N}) -> N == RoomName end, ets:lookup_element(state, rooms, 2)),
    io:format("Room is ~p \n", [Room]),
    case Room of
        [] ->
            io:format("Room not found\n"),
            State;
        _ ->
            Members = (hd(Room))#room.members,
            NewList = lists:map(fun(Element) ->
                % Extract the property from the element
                {ok, {RemoteIP, RemotePort}} = inet:peername(Element),
                % Return the property
                RemotePort
                end, Members),

            io:format("Members of the room ~p: ~p\n", [RoomName, NewList]),
            gen_tcp:send(Socket, io_lib:format("Members of the room ~p: ~p\n", [RoomName, NewList])),
            State
    end.

find_socket_by_port(Username, Room) ->
    lists:filter(fun(Socket) ->
        case inet:peername(Socket) of
            {ok, {_RemoteIP, RemotePort}} ->
                case integer_to_list(RemotePort) == Username of
                    true -> true;
                    _ -> false
                end;
            _ -> false
        end
    end, (hd(Room))#room.members).
