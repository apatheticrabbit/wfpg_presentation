-module(hyptxt).
-export([wrap_document/1,http_status/1,parse_request/1]).

%% Return an iolist to send as the http response content
%% -spec wrap_document(iolist()) -> iolist()
wrap_document(Doc) ->
    ["\r\n<html><body>",Doc,"</body></html>\r\n"].

%% The status line for an http response code
%% -spec http_status(atom()) -> iolist().
http_status(Code) when is_atom(Code) ->
    case Code of 
        ok -> ["HTTP/1.1 200 OK\r\n"];
        not_found -> ["HTTP/1.1 404 NOT FOUND\r\n"];
        no_content -> ["HTTP/1.1 204 DOC STORED\r\n"];
        _ -> ["HTTP/1.1 501 NOT IMPLEMENTED\r\n"]
    end.

%% Parse an http request
%% -spec(parse_request(binary()) ->
%%      {post,{Key,Doc}} | {get, Key}
parse_request(<<"POST /",Rest/bitstring>>) ->
    get_post_content(Rest);
parse_request(<<"GET /",Rest/bitstring>>) ->
    {get,get_path(Rest)}.

%% Get the key as an md5 hash
get_path(Bin) ->
    erlang:md5(hd(binary:split(Bin, <<" ">>))).

%%  Return the content part of the post http request
get_post_content(Bin) ->
    Content = hd(tl(binary:split(Bin,<<"\r\n\r\n">>))),
    case parse_content(binary:split(Content,<<"&">>),{no_key,no_value}) of
        {K, V} when K =:= no_key;V =:= no_value -> 
            exit(bad_post_request);
        Pair ->
            {post, Pair}
    end.

%% Parse the key value pairs in the post content
parse_content([], Acc) -> Acc;
parse_content([H|Rest], {K,V}) ->
    case H of
        <<"key=",Bin/bitstring>> ->
            Key = erlang:md5(Bin),
            parse_content(Rest,{Key,V});
        <<"doc=",Bin/bitstring>> ->
            Doc = binary:replace(Bin,<<"+">>,<<" ">>,[global]),
            Doc2 = http_uri:decode(binary_to_list(Doc)),
            parse_content(Rest,{K,Doc2});
        _ ->
            parse_content(Rest,{K,V})
    end.
