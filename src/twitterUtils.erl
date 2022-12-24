%%%-------------------------------------------------------------------
%%% @author ayushkumar
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Nov 2022 12:54 AM
%%%-------------------------------------------------------------------
-module(twitterUtils).
-author("ayushkumar").

%% API
-export([generate_next_uid/1, generate_random_tweet_identifier/0, get_random_uid_from_activeUsers/1, generate_random_hashtag/0]).

generate_next_uid(Map) ->
  GeneratedUid = binary_to_list(base64:encode(crypto:strong_rand_bytes(16))),
  InsertedUid = maps:get(GeneratedUid, Map, null),
  if InsertedUid == null -> GeneratedUid;
    true ->
      generate_next_uid(Map)
  end.

get_random_uid_from_activeUsers(ActiveUids) ->
  lists:nth(rand:uniform(length(ActiveUids)), ActiveUids).


generate_random_tweet_identifier() ->
  binary_to_list(base64:encode(crypto:strong_rand_bytes(32))).

generate_random_hashtag() ->
  "#" + binary_to_list(base64:encode(crypto:strong_rand_bytes(8))).



