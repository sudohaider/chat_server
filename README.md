# Chat Server

This Erlang module implements a simple chat server that listens for connections on a certain port and handles incoming client connections.

## Building the Project

To build the project, you need to have Erlang and rebar3 installed on your system.

1. Clone the repository to your local machine:

    ```bash
    git clone https://github.com/sudohaider/chat_server.git
    ```

2. Navigate to the project directory:

    ```bash
    cd chat_server
    ```

3. Compile the project using rebar3:

    ```bash
    rebar3 compile
    ```

## Running the Server

To run the chat server, follow these steps:

1. Ensure that the project is compiled (see "Building the Project" section).

2. Start the Erlang shell:

    ```bash
    rebar3 shell
    ```

3. In the Erlang shell, start the chat server by executing the following command:

    ```erlang
    chat_server:start().
    ```
This will start the server listening on port 1234.

4. In another command terminal (client 1), execute the following command to connect to the chat server using Telnet:

```bash
telnet localhost 1234
```

This command will attempt to establish a TCP/IP connection to the chat server running on localhost (your own computer) on port 1234. If successful, you'll be connected to the chat server and can interact with it. 

Repeat this step for multiple clients.

## Using the Server

To use the individual functionalities of the chat server, follow these steps:

1. **Create a Room**: To create a chat room, send a command in the format "CREATE RoomName" to the server. For example:
    ```
    CREATE Room1
    ```

2. **Destroy a Room**: To destroy a chat room, send a command in the format "DESTROY RoomName" to the server. For example:
    ```
    DESTROY Room1
    ```

3. **List Available Rooms**: To list all available chat rooms, send the command "LIST" to the server. For example:
    ```
    LIST
    ```

4. **Join a Room**: To join a chat room, send a command in the format "JOIN RoomName" to the server. For example:
    ```
    JOIN Room1
    ```

5. **Leave a Room**: To leave a chat room, send a command in the format "LEAVE RoomName" to the server. For example:
    ```
    LEAVE Room1
    ```

6. **Send a Message**: To send a message to a chat room, send a command in the format "SEND RoomName Message" to the server. For example:
    ```
    SEND Room1 Hello
    ```

7. **List Members**: To list all members of a specified chat room, send a command in the format "LISTMEMBERS RoomName" to the server. For example:
    ```
    LISTMEMBERS Room1
    ```

8. **Send Private Message**: After knowing the names of members using the previous command, you can send a private message to a member in the same chat room by sending the command "SENDPRIVATE RoomName UserName Message" to the server. For example:
    ```
    SENDPRIVATE Room1 User1 Hello
    ```

Remember to replace "RoomName", "UserName" and "Message" with the appropriate values for your chat room, user and message. Additionally, ensure that the server is running and properly configured to handle client connections.

## Notes

- This is a basic example and may not be suitable for production use.
- Error handling and robustness features are not included for simplicity.

