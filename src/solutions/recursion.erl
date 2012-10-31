-module(recursion).
-export([sevens/1,sevens_tail/1]).
-export([sevens_1_test/0,sevens_2_test/0,test_doc/0]).
-export([new_store/0,get_doc/2,put_doc/3]).

%% Write a function that takes a list of numbers
%% and returns a list of the different positions in
%% a list the number 7 appears. Start the positions at 0.
%% Try writing sevens in terms of a helper function
%% sevens/2 that takes the list and a position counter
%% variable.
%% -spec sevens(list(integer())) -> list(integer())
sevens(List) ->
    sevens(List, 0).
%% In the first case handle the
%% case that the list is empty
sevens([],_) -> [];
%% Next handle the case that 
%% the head of the list is a 7 
%% ex. [7|Rest]
sevens([7|Rest], N) ->
    [N | sevens(Rest,N+1)];
%% In the last case handle the case
%% where the head is any other element
sevens([_|Rest], N) ->
    sevens(Rest, N+1).

sevens_1_test() ->
    [1,4,5] = sevens([3,7,6,6,7,7]).
sevens_2_test() ->
    [1,4,5] = sevens_tail([3,7,6,6,7,7]).

%% Write sevens again but this time using tail recursion
%% Hint: As before you can define another function sevens_tail/3 
%% that takes an extra parameter to use as a counter plus 
%% another to use as a list accumulator. Notice that if
%% you build the accumulator list by prepending elements
%% it will be in reverse. You can use lists:reverse/1 to
%% fix that.
%% -spec sevens_tail(list(integer())) -> list(integer())
sevens_tail(List) ->
    sevens_tail(List, 0, []).

sevens_tail([],_,Acc) ->
    lists:reverse(Acc);
sevens_tail([7|Rest], N, Acc) ->
    sevens_tail(Rest, N+1, [N|Acc]);
sevens_tail([_|Rest], N, Acc) ->
    sevens_tail(Rest, N+1, Acc).

%% Create a document store. This will be defined by a public
%% api of 3 functions, new_store/0 , get_doc/2 and put_doc/3
%% -type doc_store(Key, Doc) :: [{Key,Doc}]

%% Returns a new document store.
%% -spec new_store() -> doc_store()
new_store() ->
    [].

%% Add a document and key to the head of the list
%% -spec put_doc(doc_store(Key,Doc),Key,Doc) -> ok
put_doc(Store,Key,Doc) ->
    [{Key,Doc}|Store].

%% Recursively look for a key match if found return 
%% that document in a tuple {ok, Doc} if you don't find
%% the document by the time the list is empty return
%% the atom not_found. Remember the document store is a list
%% of Tuples {Key,Doc},
%% -spec get_doc(doc_store(Key,Doc),Key) -> {ok,Doc} | not_found
get_doc([],_) ->
    not_found;
get_doc([{Key,Doc}],Key) ->
    {ok, Doc};
get_doc([_|Rest],Key) ->
    get_doc(Rest,Key).

test_doc() ->
    S = new_store(),
    S1 = put_doc(S,1,"testing"),
    [{1,"testing"}] = S1,
    {ok,"testing"} = get_doc(S1,1).
