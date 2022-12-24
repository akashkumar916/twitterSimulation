%%%-------------------------------------------------------------------
%%% @author ayushkumar
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Nov 2022 12:43 AM
%%%-------------------------------------------------------------------
-module(twitterClient).
-author("ayushkumar").

-behaviour(gen_server).

%% API
-export([start_link/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(twitterClient_state, {uid, pid, dbPid, genratedHashTags, allClientUids}).

%%% this is the client process that simulates actions of any twitter client
%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)

start_link(Uid, DbPid, GeneratedHashTags, AllClientsUids) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Uid, DbPid, GeneratedHashTags, AllClientsUids], []).

stop() ->
  io:format(" ~p Stopping this client. ~n", [#twitterClient_state.uid]),
  gen_server:stop(?MODULE).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #twitterClient_state{}} | {ok, State :: #twitterClient_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([Uid, DbPid, GeneratedHashTags, AllClientsUids]) ->
  TwitterClientState = #twitterClient_state{
    uid = Uid,
    pid = self(),
    dbPid = DbPid,
    genratedHashTags = GeneratedHashTags,
    allClientUids = AllClientsUids
  },
  gen_server:call(#twitterClient_state.dbPid, {registerUser, Uid, self()}),
  {ok, TwitterClientState}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #twitterClient_state{}) ->
  {reply, Reply :: term(), NewState :: #twitterClient_state{}} |
  {reply, Reply :: term(), NewState :: #twitterClient_state{}, timeout() | hibernate} |
  {noreply, NewState :: #twitterClient_state{}} |
  {noreply, NewState :: #twitterClient_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #twitterClient_state{}} |
  {stop, Reason :: term(), NewState :: #twitterClient_state{}}).

handle_call({follow, UidToFollow}, _From, State = #twitterClient_state{uid = Uid, pid = Pid}) ->
  {ok, DoesAlreadyFollow} = gen_server:call(#twitterClient_state.dbPid, {alreadyfollow, UidToFollow, Uid}),
  if DoesAlreadyFollow == true -> {reply, {alreadyFollows, UidToFollow}, State};
    true ->
      gen_server:call(#twitterClient_state.dbPid, {addfollower, Uid, UidToFollow}),
      {reply, followSuccess, State}
  end;


handle_call({goOffline}, _From, State = #twitterClient_state{uid = Uid, pid = Pid}) ->
  gen_server:call(#twitterClient_state.dbPid, {takeuseroffline, Uid}),
  {reply, {wentOffline, Uid}, State};

handle_call({comeOnline}, _From, State = #twitterClient_state{uid = Uid, pid = Pid}) ->
  gen_server:call(#twitterClient_state.dbPid, {setuseronline, Uid}),
  {reply, {cameOnline, Uid}, State};

handle_call({receivedTweet, Tweet, Uid}, _From, State) ->
  io:format(" ~p User received tweet = ~p, from user = ~p. ~n", [#twitterClient_state.uid, Tweet, Uid]),
  {noreply, State};

handle_call({receivedRetweet, Tweet, Uid}, _From, State) ->
  io:format(" ~p User received tweet = ~p, from user = ~p. ~n", [#twitterClient_state.uid, Tweet, Uid]),
  {noreply, State};

handle_call({makeTweet, Type}, _From, State = #twitterClient_state{uid = Uid, pid = Pid}) ->
  Tweet = generate_self_tweet(Type),
  TweetId = crypto:hash(md5, Tweet),
  %% make DB call gen_server, insert in global maps {tweet -> tweetId}, {tweetId -> tweet}, save it in self_tweets, save in feed of all followers + send this tweet to online followers.
  gen_server:call(#twitterClient_state.dbPid, {addtweets, Uid, Tweet}),
  {ok, LiveFollowersList} = gen_server:call(#twitterClient_state.dbPid, {livefollowers, Uid}),
  propagate_tweet_to_live_followers(0, LiveFollowersList, Tweet),
  {reply, {madeTweet, Uid, Tweet}, State};

handle_call({retweet}, _From, State = #twitterClient_state{uid = Uid, pid = Pid}) ->
  AllFeedTweetIds = gen_server:call(#twitterClient_state.dbPid, {gettweets, Uid}),
  %% my feed will contain tweetIds of all the tweets made by users that I follow
  RandomTweet = lists:nth(rand:uniform(length(AllFeedTweetIds)), AllFeedTweetIds),
  %% make db call here to get tweet string from tweetId.
  %% make db call, insert this tweet in my retweeted tweets, and in my followers feed
  %% make DB call, gen_server, create tweetId, save it in self_tweets, save in feed of all followers + send this tweet to online followers.
  {ok, LiveFollowersList} = gen_server:call(#twitterClient_state.dbPid, {livefollowers, Uid}),
  propagate_retweet_to_live_followers(0, LiveFollowersList, RandomTweet),
  {reply, {madeRetweet, Uid, RandomTweet}, State};

handle_call({alreadyRegistered, Uid}, _From, State) ->
  io:format(" ~p User already registered. ~n", [Uid]),
  {noreply, State};

handle_call({registerationSuccess, Uid}, _From, State) ->
  io:format(" ~p User registered successfully. ~n", [Uid]),
  {noreply, State}.
%% load all the tweets it missed.

propagate_tweet_to_live_followers(CurrIdx, LiveFollowersList, Tweet) ->
  if CurrIdx == length(LiveFollowersList) -> {};
    true ->
      CurrUid = lists:nth(CurrIdx, LiveFollowersList),
      gen_server:call(CurrUid, {receivedTweet, Tweet, #twitterClient_state.uid}),
      propagate_tweet_to_live_followers(CurrIdx + 1, LiveFollowersList, Tweet)
  end.

generate_self_tweet(Type) -> %% 0 -> with hashtag, 1 -> with mention, 2 -> both, 3/_ -> default
  BaseTweet = string:concat("Hi, this is a base tweet for my uid ", #twitterClient_state.uid),
  case Type of
    0 ->
      BaseTweet + lists:nth(rand:uniform(length(#twitterClient_state.genratedHashTags)), #twitterClient_state.genratedHashTags);
    1 ->
      BaseTweet + "@" + lists:nth(rand:uniform(length(#twitterClient_state.allClientUids)), #twitterClient_state.allClientUids);
    2 -> BaseTweet +
      lists:nth(rand:uniform(length(#twitterClient_state.genratedHashTags)), #twitterClient_state.genratedHashTags) +
      "@" + lists:nth(rand:uniform(length(#twitterClient_state.allClientUids)), #twitterClient_state.allClientUids);
    _ -> BaseTweet
  end.


propagate_retweet_to_live_followers(CurrIdx, LiveFollowersList, ReTweet) ->
  if CurrIdx == length(LiveFollowersList) -> {};
    true ->
      CurrUid = lists:nth(CurrIdx, LiveFollowersList),
      gen_server:call(CurrUid, {receivedRetweet, ReTweet, #twitterClient_state.uid}),
      propagate_retweet_to_live_followers(CurrIdx + 1, LiveFollowersList, ReTweet)
  end.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #twitterClient_state{}) ->
  {noreply, NewState :: #twitterClient_state{}} |
  {noreply, NewState :: #twitterClient_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #twitterClient_state{}}).
handle_cast(_Request, State = #twitterClient_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #twitterClient_state{}) ->
  {noreply, NewState :: #twitterClient_state{}} |
  {noreply, NewState :: #twitterClient_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #twitterClient_state{}}).
handle_info(_Info, State = #twitterClient_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #twitterClient_state{}) -> term()).
terminate(_Reason, _State = #twitterClient_state{}) ->
  io:format("Shutting down this client.~n"),
  terminated.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #twitterClient_state{},
    Extra :: term()) ->
  {ok, NewState :: #twitterClient_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #twitterClient_state{}, _Extra) ->
  {ok, State}.