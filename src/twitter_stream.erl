-module(twitter_stream).
-author('jebu@jebu.net').
 
%% API
-export([fetch/1, fetch/3, process_data/1]).
 
% single arg version expects url of the form http://katsudonchef:fatwhitedevil@stream.twitter.com/1/statuses/filter.json
% this will spawn the 3 arg version so the shell is free
fetch(URL) ->
    application:start(inets),
    spawn(twitter_stream, fetch, [URL, 5, 30]).
 
% 3 arg version expects url of the form http://user:password@stream.twitter.com/1/statuses/sample.json  
% retry - number of times the stream is reconnected
% sleep - secs to sleep between retries.
fetch(URL, Retry, Sleep) when Retry > 0 ->
  % setup the request to process async
  % and have it stream the data back to this process
  post_status("Sending request ~s~n", [URL]),
  try http:request(post, 
                    {URL, [], "application/x-www-form-urlencoded", "track=#fb"},
                    [], 
                    [{sync, false}, 
                     {stream, self}]) of
    {ok, RequestId} ->
          post_status("Receiving chunks for ~p~n", [RequestId]),
      case receive_chunk(RequestId) of
        {ok, _} ->
              post_status("waiting and then retrying~n"),
          % stream broke normally retry 
          timer:sleep(Sleep * 1000),
          fetch(URL, Retry - 1, Sleep);
        {error, unauthorized, Result} ->
              post_status("unauthorized :( ~p", [Result]),
          {error, Result, unauthorized};
        {error, timeout} ->
              post_status("Timed out, sleeping a bit~n"),
          timer:sleep(Sleep * 1000),
          fetch(URL, Retry - 1, Sleep);
        {_, Reason} ->
          post_status("Got some Reason ~p ~n", [Reason]),
          timer:sleep(Sleep * 1000),
          fetch(URL, Retry - 1, Sleep)
      end;
    _ ->
          post_status("not ok, retrying~n"),
      timer:sleep(Sleep * 1000),
      fetch(URL, Retry - 1, Sleep)
  catch
      Error:Reason ->
          post_status("FUUUUUUUUUUUUUU~~ ~p ~p~n", [Error, Reason]),
          exit(ok)
  end;
%
fetch(_, Retry, _) when Retry =< 0 ->
  post_status("No more retries done with processing fetch thread~n"),
  {error, no_more_retry}.
%
% this is the tweet handler persumably you could do something useful here
%
process_data(Data) ->
    %% post_status("Received tweet ~p ~n", [Data]),
    Room = jazzhose_web:get_the_room(),
    Room ! {self(), tweet, Data},
  ok.
 
post_status(Format, Data) ->
    post_status(io_lib:format(Format, Data)).

post_status(Data) ->
    io:format("Status: ~s~n", [Data]),
    %% jazzhose_web:get_the_room() ! {self(), status, list_to_binary(io_lib:format("Status: ~s", [Data]))},
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
receive_chunk(RequestId) ->
  receive
    {http, {RequestId, {error, Reason}}} when(Reason =:= etimedout) orelse(Reason =:= timeout) -> 
      {error, timeout};
    {http, {RequestId, {{_, 401, _} = Status, Headers, _}}} -> 
      {error, unauthorized, {Status, Headers}};
    {http, {RequestId, Result}} -> 
      {error, Result};
 
    %% start of streaming data
    {http,{RequestId, stream_start, Headers}} ->
      post_status("Streaming data start ~p ~n",[Headers]),
      receive_chunk(RequestId);
 
    %% streaming chunk of data
    %% this is where we will be looping around, 
    %% we spawn this off to a seperate process as soon as we get the chunk and go back to receiving the tweets
    {http,{RequestId, stream, Data}} ->
      spawn(twitter_stream, process_data, [Data]),
      receive_chunk(RequestId);
 
    %% end of streaming data
    {http,{RequestId, stream_end, Headers}} ->
      post_status("Streaming data end ~p ~n", [Headers]),
      {ok, RequestId};
      {ping} -> 
          post_status("Pong"),
          receive_chunk(RequestId)
  %% timeout
  after 60 * 1000 ->
    {error, timeout}
 
  end.
