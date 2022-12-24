
-module(twitterServer).

-behaviour(gen_server).

%% API
-export([start_link/0, handle_cast/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, terminate/2, code_change/3]).

-export([register_user/2,disconnect_user/1,add_follower/2,add_mentions/2,add_subscriber/2,
  add_Tweets/2,get_follower/1,get_my_Tweets/1,get_subscribed_to/1,add_hastags/2, is_user_online/1, set_user_online/1,take_user_offine/1,already_follows/2]).

-export([get_follower_by_user/2, get_most_subscribed_users/1, loop_hastags/3, loop_mentions/3,get_active_user/1,check_active_user/3, get_retweets/1, get_tweet_list/2]).

-define(SERVER, ?MODULE).

-record(twitterServer_state, {uid, pid, subscriber, tag, tweets,retweets, mentions, useridlist, followerid}).
%% Here tweets is tweet Id and mentions is mentions id and sub is Subscriber Id

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
  {ok, self()}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #twitterServer_state{}} | {ok, State :: #twitterServer_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).

init([]) ->
  %%ETS Table -> Client registry, Tweets, hashtag_mentions, follower, subscribed_to
  ets:new(clients_registry, [set, public, named_table]),
  ets:new(tweets, [set, public, named_table]),
  ets:new(hashtags_mentions, [set, public, named_table]),
  ets:new(subscribed_to, [set, public, named_table]),
  ets:new(followers, [set, public, named_table]),
  ets:new(active_user, [set, public, named_table]),
  ets:new(retweets, [set, public, named_table]),
  {ok, #twitterServer_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #twitterServer_state{}) ->
  {reply, Reply :: term(), NewState :: #twitterServer_state{}} |
  {reply, Reply :: term(), NewState :: #twitterServer_state{}, timeout() | hibernate}|
  {reply, Reply :: term(), NewState :: #twitterServer_state{}} |
  {reply, Reply :: term(), NewState :: #twitterServer_state{}, timeout() | hibernate}|
  {reply, Reply :: term(), NewState :: #twitterServer_state{}} |
  {reply, Reply :: term(), NewState :: #twitterServer_state{}, timeout() | hibernate}|
  {reply, Reply :: term(), NewState :: #twitterServer_state{}} |
  {reply, Reply :: term(), NewState :: #twitterServer_state{}} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #twitterServer_state{}} |
  {stop, Reason :: term(), NewState :: #twitterServer_state{}}).

%% For registering the User
handle_call({registeruser, UserId, Pid}, _From, State = #twitterServer_state{uid=UserId, pid=Pid}) ->
  register_user(#twitterServer_state.uid, Pid),
  {reply, ok, State};
%% For adding the follower
handle_call({addfollower, UserId, Sub}, _From, State = #twitterServer_state{uid=UserId, subcribers = Sub}) ->
  add_follower(#twitterServer_state.uid,#twitterServer_state.subcribers),
  {reply, ok, State};

%% For adding the tweets of the given User
handle_call({addtweets, UserId, Tweets}, _From, State = #twitterServer_state{uid=UserId, tweets =Tweets}) ->
  add_Tweets(#twitterServer_state.uid, #twitterServer_state.tweets),
  {reply, ok, State};
%% For adding the tweets of the given User
handle_call({addretweets, UserId, Retweets}, _From, State = #twitterServer_state{uid=UserId, retweets =Retweets}) ->
  add_Retweets(#twitterServer_state.uid, #twitterServer_state.retweets),
  {reply, ok, State};
handle_call({livefollowers, UserId}, _From, State = #twitterServer_state{uid=UserId}) ->
  {reply, {ok, get_active_user(UserId)}, State};
%% For adding in the hashtags tuple corresponding to the user
handle_call({addhastags, UserId, Tag}, _From, State = #twitterServer_state{uid=UserId, tag =Tag}) ->
  add_hastags(#twitterServer_state.tag, #twitterServer_state.uid),
  {reply, ok, State};
%% For adding in the mentions corresponding to the user
handle_call({addmentions, UserId, Mentions}, _From, State = #twitterServer_state{uid=UserId, mentions = Mentions}) ->
  add_mentions(#twitterServer_state.mentions,#twitterServer_state.uid),
  {reply, ok, State};
%% setting the user active
handle_call({setuseronline, UserId}, _From, State = #twitterServer_state{uid=UserId}) ->
  set_user_online(#twitterServer_state.uid),
  {reply, ok, State};
%% setting the user inactive
handle_call({takeuseroffline,UserId}, _From, State = #twitterServer_state{uid=UserId}) ->
  take_user_offine(#twitterServer_state.uid),
  {reply, ok, State};

%%get the number of subscriber
handle_call({getsubscriber, UserId}, _From, State = #twitterServer_state{uid=UserId}) ->
  get_subscribed_to(#twitterServer_state.uid),
  {reply, ok, State};
%%get the number of follower
handle_call({getfollower, UserId}, _From, State = #twitterServer_state{uid=UserId}) ->
  get_follower(#twitterServer_state.uid),
  {reply, ok, State};
%%get the number of tweets
handle_call({gettweets, UserId}, _From, State = #twitterServer_state{uid=UserId}) ->
  get_my_Tweets(#twitterServer_state.uid),
  {reply, ok, State};

%%get the number of Retweets
handle_call({gettweets, UserId}, _From, State = #twitterServer_state{uid=UserId}) ->
  get_retweets(#twitterServer_state.uid),
  {reply, ok, State};

%% get the user List for ZIPf distribution
handle_call({subscriberlist, UserIdList}, _From, State = #twitterServer_state{ useridlist =UserIdList}) ->
  get_most_subscribed_users(#twitterServer_state.useridlist),
  {reply, ok, State};
%% checking the user follows or not
handle_call({alreadyfollow, UserId, FollowerId}, _From, State = #twitterServer_state{uid=UserId, followerid=FollowerId}) ->
  {reply, {ok, already_follows(#twitterServer_state.uid, #twitterServer_state.followerid)}, State};
%% is user online ?
handle_call({useronline, UserId}, _From, State = #twitterServer_state{uid=UserId}) ->
  is_user_online(#twitterServer_state.uid),
  {reply, ok, State};

handle_call({activeuser, UserId}, _From, State = #twitterServer_state{uid=UserId}) ->
  get_active_user(#twitterServer_state.uid),
  {reply, ok, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #twitterServer_state{}) -> term()).
terminate(_Reason, _State = #twitterServer_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #twitterServer_state{},
    Extra :: term()) ->
  {ok, NewState :: #twitterServer_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #twitterServer_state{}, _Extra) ->
  {ok, State}.


%%%===================================================================
%%% Internal functions - Database Handler
%%%===================================================================

register_user(UserId,Pid) ->
  ets:insert(clients_registry, {UserId, Pid}),
  ets:insert(tweets, {UserId, []}),
  ets:insert(subscribed_to, {UserId, []}),
  ets:insert(active_user, {UserId, #{}}),
  FollowerTuple = ets:lookup(followers, UserId),
  if FollowerTuple == [] ->
    ets:insert(followers, {UserId, []});
    true->{}
  end.

%%   Disconnect the User
disconnect_user(UserId)->
  ets:insert(clients_registry,{UserId,"NULL"}).

%%   Getter Functions - for getting the Tweets, subscriber and follower
get_my_Tweets(UserId)->
  TweetsTuple = ets:lookup(tweets,UserId),
  {TweetsTuple}.

get_tweet_list(TweetUserList,[Id | UserId])->
  TweetsTuple= get_my_Tweets(Id),
  TweetUserList = [ TweetsTuple | TweetUserList],
  get_tweet_list(TweetUserList, UserId).

%% getting tweet list for retweets

get_retweets(UserId)->
  SubscriberToList = get_subscribed_to(UserId),
  TweetList = [],
  get_tweet_list(TweetList, SubscriberToList),
  {TweetList}.

get_subscribed_to(UserId)->
  SubscriberTuple=ets:lookup(subscribed_to,UserId),
  {SubscriberTuple}.

get_follower(UserId)->
  FollowerTuple = ets:lookup(subscribed_to,UserId),
  {FollowerTuple}.


%%  Setter Functions - to set the subscriber, follower and processing the Tweets
add_subscriber(UserId,Sub)->
  [Tup] = ets:lookup(subscribed_to, UserId),
  List = [Sub | Tup],
  ets:insert(subscribed_to, {UserId, List}).

add_follower(UserId,Sub)->
  [Tup] = ets:lookup(subscribed_to, UserId),
  List = [Sub | Tup],
  ets:insert(subscribed_to, {UserId, List}).


%%helper Function
loop_hastags([T|Tag],UserId, Tweets)->
  add_hastags(T,UserId),
  loop_hastags(Tag, UserId, Tweets).

loop_mentions([T|Mentions],UserId, Tweets)->
  add_mentions(T,UserId),
  loop_mentions(Mentions, UserId, Tweets).

%% Processing the Tweets
add_Tweets(UserId, Tweets)->
  TweetsTuple=ets:lookup(tweet, UserId),
  ets:insert(tweet,{UserId,[TweetsTuple|Tweets]}),
  %% Hashtag list
  HashtagList= re:compile("~r#[a-zA-Z0-9_]+",Tweets),
  loop_hastags([HashtagList],UserId, Tweets),
  %% Mentions list
  MentionList = re:compile("~r@[a-zA-Z0-9_]+",Tweets),
  loop_hastags([MentionList],UserId, Tweets).

%% Processing the Tweets
add_Retweets(UserId, Retweets)->
  RetweetsTuple=ets:lookup(retweet, UserId),
  ets:insert(tweet,{UserId,[RetweetsTuple|Retweets]}),
  %% Hashtag list
  HashtagList= re:compile("~r#[a-zA-Z0-9_]+",Retweets),
  loop_hastags([HashtagList],UserId, Retweets),
  %% Mentions list
  MentionList = re:compile("~r@[a-zA-Z0-9_]+",Retweets),
  loop_hastags([MentionList],UserId, Retweets).

add_hastags(Tag, UserId)->
  HashtagTuple=ets:lookup(hashtags_mentions, Tag),
  AddHashtag=[HashtagTuple| Tag],
  ets:insert(hashtags_mentions,{UserId,AddHashtag}).

add_mentions(Mentions,UserId)->
  MentionsTuple=ets:lookup(hashtags_mentions, Mentions),
  AddMentions=[MentionsTuple| Mentions],
  ets:insert(hashtags_mentions,{UserId,AddMentions}).

%%ZIPf Distribution helper functions
count_subscriber(Follower) -> length([X || X <- Follower, X < 1]).

get_follower_by_user([First |UserIdList], ZipfUserMap)->
  maps:put(First, count_subscriber(get_follower(First)), ZipfUserMap),
  get_follower_by_user([UserIdList], ZipfUserMap).

get_most_subscribed_users(UserIdList)->
  %%Map where user Id is sorted according to the count of their subscribers
  ZipfUserMap = #{},
  get_follower_by_user(UserIdList,[ZipfUserMap]),

  %% Map to List, for sorting on the value of map ( no. of follower)
  List = maps:to_list(ZipfUserMap),
  lists:keysort(2, List),
  {List}.

is_user_online(UserId) ->
  ActiveUserMap = ets:lookup(active_user,UserId),
  {maps:get(UserId, ActiveUserMap)}.

set_user_online(UserId) ->
  ActiveUserMap = ets:lookup(active_user,UserId),
  maps:put(UserId, 1, ActiveUserMap).

take_user_offine(UserId) ->
  ActiveUserMap = ets:lookup(active_user,UserId),
  maps:put(UserId, 0, ActiveUserMap).

%% finding in the follower tuple
find_in_follower_tuple(_, []) -> false;

find_in_follower_tuple(E, T) when is_tuple(T) ->
  find_in_follower_tuple(E, tuple_to_list(T));

find_in_follower_tuple(E, [H|T]) ->
  case find_in_follower_tuple(E, H) of
    false -> find_in_follower_tuple(E, T);
    true -> true
  end.

already_follows(Celeb, Follower) ->
  FollowerTuple = get_follower(Celeb),
  find_in_follower_tuple(FollowerTuple,Follower).

check_active_user(ActiveUserMap, [F| FollowerTuple],ActiveUserList)->
  Exists = maps:get(F,ActiveUserMap,null),
  if Exists =/= null ->
    ActiveUserList = [F| ActiveUserList]
  end,
  check_active_user(ActiveUserMap, FollowerTuple, ActiveUserList).

get_active_user(UserId)->
  FollowerTuple = get_follower(UserId),
  ActiveUserMap = ets:lookup(active_user, UserId),
  ActiveUserList= [],
  check_active_user(ActiveUserMap, FollowerTuple, ActiveUserList),
  {ActiveUserList}.

handle_cast(_Request, State = #twitterServer_state{}) ->
  {noreply, State}.
