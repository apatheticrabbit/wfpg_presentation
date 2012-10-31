%% Proc demo module some functions to show Erlang processes in action.
-module(proc_demo).
-export([printer_loop/0,brave_loop/0]).

% This is a main process loop that prints anything it recieves
printer_loop() ->
    receive
        Any ->
            io:format("I got a message!~n~p~n",[Any]),
            printer_loop()
    end.

% This is a process loop that ends in an error if you send
% the correct message
brave_loop() ->
    receive
        spiders ->
            io:format("Oh no not that!~n"),
            exit(please_no_spiders);
        Any ->
            io:format("I aint afraid of no ~p!~n",[Any]),
            brave_loop()
    end.
