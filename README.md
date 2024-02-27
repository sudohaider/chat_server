# Chat Server

This is a simple chat server application written in Erlang.

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

4. In the Erlang shell, stop the chat server by executing the following command:

    ```erlang
    chat_server:stop().
    ```

## Testing the Project

To run the test, execute the following command:

 ```erlang
 rebar3 eunit
 ```
