-module(webs).
-export([start/1, acceptor/1]).

%% Start a new TCP acceptor at port number PortNum
start(PortNum) ->
    {ok, Listen} = start_listener(PortNum),
    spawn(?MODULE, acceptor, [Listen]).

%% Block waiting for a request and spawn a
%% new acceptor when one is received
acceptor(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    spawn(?MODULE, acceptor, [Listen]),
    io:format("Got a request~n",[]),
    handle_request(Socket).

%% Parse the tcp requests
handle_request(Socket) ->
    % We turn the socket active to start receiving messages
    % from the TCP socket.
    inet:setopts(Socket,[{active,true}]),
    receive
        {tcp, _Socket, Data} ->
            %% Use hyptxt:parse_request/1 on Data to get either
            %% {get, Key}:
                %% Try to use doc_serv:get/2
                %% to get {ok, Doc} or not_found.
                %% {ok, Doc}: 
                    %% return the ok status (hyptxt:http_status) along with
                    %% Doc wrapped using hyptxt:wrap_document
                %% not_found:
                    %% return the not found status with a informative
                    %% message wrapped using hyptxt:wrap_document
            %% {post, {Key,Doc}:
                %% use doc_serv:put/2 to store the document and
                %% return no_content using hyptxt:http_status/1
            %% HINT: Remember doc_serv:get and put take the Pid of the sending process
            %% to get the pid of this process use the self() function.
            Msg = ???,
            %% Print out the message for debugging
            io:format("~p~n",[Msg]),
            gen_tcp:send(Socket,Msg),
            gen_tcp:close(Socket);
        {tcp_closed, _ } -> ok;
        Any ->
            io:format("Got Other Message ~p",[Any])
    end.

%% Start a TCP listener socket in an inactive state
%% it won't receive messages until we turn it active
start_listener(PortNum) ->
    gen_tcp:listen(PortNum,[binary, {active, false}]).
