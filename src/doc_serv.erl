-module(doc_serv).
-export([start/0,put/3,get/2]).
-export([init/0]).

%% Start the document server process
%% register the server with its module name
%% -spec start() -> started.
start() ->
    case whereis(?MODULE) of
        undefined ->
            register(?MODULE,spawn(?MODULE,init,[])),
            started;
        _ ->
            started
    end.

%% Initialize the main loop.
%% our state will just be one variable
%% a document store from the recursion module
%% create a new document store and pass it to
%% the main loop
init() ->
    undefined.

%% A function for calling processes to put
%% documents into the store
put(From, Key, Document) ->
    ?MODULE ! {put, From, {Key, Document}}.

%% A function for calling processes to get
%% documents from the store
get(From, Key) ->
    ?MODULE ! {get, From, Key},
    %% Using a receive here causes the calling
    %% process to halt and wait for our reply
    %% turning this call into a synchronous one
    receive
        Reply -> Reply
    end.

%% The main server loop. This should be tail 
%% recursive. Call receive and wait for a message
%% the messages you should handle will be the ones
%% from put/3, get/2 as well as the message stop
%% Handle them as follows
%% {get, From, Key} : 
    %% try to get the document from the doc store S
    %% and send a response to the From process
    %% and then continue. The message should be {ok,Doc} or
    %% not_found
%% {put, From, {Key, Doc}}
    %% Just store the document in the doc store S then
    %% continue. Don't bother replying to the client.
%% stop :
    %% just return the atom stopped and do not continue
loop(S) ->
    receive
        stop -> 
            stopped;
        Other ->
            io:format("I got a message I don't understand ~p~n",[Other])
    end.
