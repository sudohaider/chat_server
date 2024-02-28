# Chat Server

This Erlang module implements a simple chat server that listens for connections on port 1234 and handles incoming client connections.

## Description

### Connecting Clients
Clients can connect to the server using a TCP/IP connection to the specified port (1234 in this case).

### Handling Client Connections
The server accepts incoming client connections and spawns a new process to handle each connection. The `accept/1` function is responsible for accepting connections, while the `handle_client/1` function is responsible for handling client messages.

## Functions

### `start/0`
Starts the chat server and listens for incoming connections on port 1234.

### `accept/1`
Accepts incoming connections on the specified socket and spawns a new process to handle each connection.

### `handle_client/1`
Handles messages received from the client socket. Prints a message when a client connects.

## Building the Project

To build the project, you need to have Erlang and rebar3 installed on your system.

1. Clone the repository to your local machine:

    ```bash
    git clone [https://github.com/alihaidersays/chat_server.git](https://github.com/alihaidersays/chat_server.git)
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

4. In another command terminal (client), execute the following command to connect to the chat server using Telnet:

```bash
telnet localhost 1234
```

This command will attempt to establish a TCP/IP connection to the chat server running on localhost (your own computer) on port 1234. If successful, you'll be connected to the chat server and can interact with it.

5. After connecting to the server, write the username in the client terminal to connect to the server and press Enter. For example:

```bash
Haider
```

Replace "Haider" with the desired username. Once you've entered the username, it will be sent to the server, and the server will handle the connection accordingly.

## Notes

- This is a basic example and may not be suitable for production use.
- Error handling and robustness features are not included for simplicity.

