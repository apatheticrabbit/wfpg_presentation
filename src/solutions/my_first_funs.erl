-module(my_first_funs).
-compile(export_all).

%% Rewrite this function so that when sent a number N
%% it returns the atom 'sonic' if that number is positive or
%% zero and returns the atom tails if negative
%% -spec sonic_or_tails(number()) -> sonic | tails
sonic_or_tails(N) when N >= 0 ->
    sonic;
sonic_or_tails(N) when N < 0 ->
    tails.

%% Write a function to turn 
%% a regular string into the body of an html document
%% for example "<html><body>Hello World<body></html>"
%% You can concatenate strings using the ++ operator
%% -spec to_html(string()) -> string()
to_html(Body) ->
    "<html><body>" ++ Body ++ "</html></body>".

%% The previous to_html function works but the
%% ++ operator is slow for large lists. To combat this Erlang
%% has the concept of iolists. These are nested lists of chars
%% (therefore strings) and binaries that are effciently flattened
%% by io operations like writing to files or sockets.
%% ex: ["hello world" ,<<"how are you?">>]
%% Make an iolist version of to_html. To do this simply
%% return the 3 parts as above in a list. This nesting bypasses
%% the need for ++ to traverse the entire list
%% -spec to_html_io(iolist()) -> iolist()
to_html_io(Body) ->
    ["<html><body>", Body, "</html></body>"].
