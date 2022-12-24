%%%-------------------------------------------------------------------
%%% @author ayushkumar
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Nov 2022 3:24 AM
%%%-------------------------------------------------------------------
-module(twitterClientSimulator).
-author("ayushkumar").

-behaviour(gen_server).

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(MAX_TWEETS, 1000000).
-define(MAX_RETWEETS, 500000).


-record(twitterClientSimulatorState, {
  uidToPidMap,
  pidToUidMap,
  allClientsPids,
  allClientsUids,
  offlineUids,
  numClients,
  numMaxFollowers,
  percentageDisconnect,
  prefilledHashtags,
  dpPid}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
start_link(NumClients, NumMaxFollowers, PercentageDisconnect) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [NumClients, NumMaxFollowers, PercentageDisconnect], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #twitterClientSimulatorState{}} | {ok, State :: #twitterClientSimulatorState{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).

init([NumClients, NumMaxFollowers, PercentageDisconnect]) ->
  {ok, DbPid} = twitterServer:start_link(),
  UpdatedTwitterClientSimulatorState = #twitterClientSimulatorState
  {allClientsPids = [],
    allClientsUids = [],
    uidToPidMap = maps:new(),
    pidToUidMap = maps:new(),
    offlineUids = [],
    prefilledHashtags = [],
    numClients = NumClients,
    numMaxFollowers = NumMaxFollowers,
    percentageDisconnect = PercentageDisconnect,
    dpPid = DbPid},
  start_all_client_nodes(NumClients, UpdatedTwitterClientSimulatorState),
  statistics(runtime),
  statistics(wall_clock),
  prefill_allowed_hashtags(NumClients * 10),
  simulate_followers(0, NumMaxFollowers, UpdatedTwitterClientSimulatorState),
  gen_server:cast(self(), takeUsersOffline),
  gen_server:cast(self(), {startTweeting, ?MAX_TWEETS}),
  gen_server:cast(self(), {startRetweeting, ?MAX_RETWEETS}),
  {_, Time1} = statistics(runtime),
  {_, Time2} = statistics(wall_clock),
  U1 = Time1 * 1000,
  U2 = Time2 * 1000,
  io:format("Time taken for initialization =~p (~p) microseconds~n",
    [U1, U2]),
  {ok, UpdatedTwitterClientSimulatorState}.


start_all_client_nodes(
    NumClientsToSpawn,
    #twitterClientSimulatorState{
      allClientsPids = AllClientsPids,
      allClientsUids = AllClientsUids,
      uidToPidMap = UidToPidMap,
      pidToUidMap = PidToUidMap,
      dpPid = DbPid,
      prefilledHashtags = PrefilledHashtags
    }
) when NumClientsToSpawn > 0 ->
  Uid = twitterUtils:generate_next_uid(UidToPidMap),
  {ok, Pid} = twitterClient:start_link(Uid, DbPid, PrefilledHashtags, AllClientsUids),
  UpdatedAllClientsPid = lists:append(AllClientsPids, Pid),
  UpdatedAllClientsUid = lists:append(AllClientsUids, Uid),
  UpdatedUidToPidMap = maps:put(Uid, Pid, UidToPidMap),
  UpdatedPidToUidMap = maps:put(Pid, Uid, PidToUidMap),
  start_all_client_nodes(NumClientsToSpawn - 1, #twitterClientSimulatorState{
    allClientsPids = UpdatedAllClientsPid,
    allClientsUids = UpdatedAllClientsUid,
    uidToPidMap = UpdatedUidToPidMap,
    pidToUidMap = UpdatedPidToUidMap}).

simulate_followers(CurrentIdx, NumFollowers, State = #twitterClientSimulatorState{
  allClientsPids = AllClientsPids,
  allClientsUids = AllClientsUids,
  uidToPidMap = UidToPidMap,
  pidToUidMap = PidToUidMap
}) ->
  if CurrentIdx == length(AllClientsUids) -> {};
    true -> increase_uid_followers(NumFollowers, lists:nth(CurrentIdx, AllClientsUids))
  end,
  simulate_followers(CurrentIdx + 1, NumFollowers / (CurrentIdx + 1), State). %% for zipf

increase_uid_followers(NumFollowers, UidToFollow) when NumFollowers > 0 ->
  RandomUid = twitterUtils:get_random_uid_from_activeUsers(#twitterClientSimulatorState.allClientsUids),
  UidPid = maps:get(RandomUid, #twitterClientSimulatorState.uidToPidMap),
  gen_server:call(UidPid, {follow, UidToFollow}),
  increase_uid_followers(NumFollowers - 1, UidToFollow).


handle_cast({takeUsersOffline}, _From, State) ->
  timer:sleep(5000),
  UidsLeftToTakeOffline = #twitterClientSimulatorState.numClients * #twitterClientSimulatorState.percentageDisconnect / 100,
  take_users_offline_internal(UidsLeftToTakeOffline),
  gen_server:cast(self(), takeUsersOffline);

handle_cast({startTweeting, MaxTweets}, _From, State) ->
  timer:sleep(100),
  RandomUid = twitterUtils:get_random_uid_from_activeUsers(#twitterClientSimulatorState.allClientsUids),
  UidPid = maps:get(RandomUid, #twitterClientSimulatorState.uidToPidMap),
  gen_server:call(UidPid, {makeTweet, MaxTweets rem 3}),
  gen_server:cast(self(), {startTweeting, MaxTweets - 1});

handle_cast({startRetweeting, MaxRetweets}, _From, State) ->
  timer:sleep(500),
  RandomUid = twitterUtils:get_random_uid_from_activeUsers(#twitterClientSimulatorState.allClientsUids),
  UidPid = maps:get(RandomUid, #twitterClientSimulatorState.uidToPidMap),
  gen_server:call(UidPid, {retweet}),
  gen_server:cast(self(), {startRetweeting, MaxRetweets - 1}).

handle_call({alreadyFollows, UidToFollow}, _From, State = #twitterClientSimulatorState{
  allClientsPids = AllClientsPids,
  allClientsUids = AllClientsUids,
  uidToPidMap = UidToPidMap,
  pidToUidMap = PidToUidMap
}) ->
  RandomUid = twitterUtils:get_random_uid_from_activeUsers(AllClientsUids),
  UidPid = maps:get(RandomUid, UidToPidMap),
  gen_server:call(UidPid, {follow, UidToFollow});

handle_call({wentOffline, Uid}, _From, State = #twitterClientSimulatorState{
  allClientsPids = AllClientsPids,
  allClientsUids = AllClientsUids,
  uidToPidMap = UidToPidMap,
  pidToUidMap = PidToUidMap,
  offlineUids = OfflineUids}) ->
  UpdatedOfflineUids = lists:append(OfflineUids, Uid),
  {noreply, #twitterClientSimulatorState{offlineUids = UpdatedOfflineUids}},
  io:format(" ~p User went offline successfully ~n", [Uid]);

handle_call({cameOnline, Uid}, _From, State = #twitterClientSimulatorState{
  allClientsPids = AllClientsPids,
  allClientsUids = AllClientsUids,
  uidToPidMap = UidToPidMap,
  pidToUidMap = PidToUidMap,
  offlineUids = OfflineUids}) ->
  UpdatedOfflineUids = lists:delete(OfflineUids, Uid),
  {noreply, #twitterClientSimulatorState{offlineUids = UpdatedOfflineUids}},
  io:format(" ~p User came online successfully ~n", [Uid]);

handle_call({madeTweet, Uid, Tweet}, _From, State) ->
  io:format(" ~p User made tweet = ~p ~n", [Uid, Tweet]);

handle_call({madeRetweet, Uid, Tweet}, _From, State) ->
  io:format(" ~p User made a retweet = ~p ~n", [Uid, Tweet]).


take_users_offline_internal(UidsLeftToTakeOffline) when UidsLeftToTakeOffline > 0 ->
  RandomUid = twitterUtils:get_random_uid_from_activeUsers(#twitterClientSimulatorState.allClientsUids),
  UidPid = maps:get(RandomUid, #twitterClientSimulatorState.uidToPidMap),
  gen_server:call(UidPid, {goOffline}),
  take_users_offline_internal(UidsLeftToTakeOffline - 1).

prefill_allowed_hashtags(NumHashTags) ->
  HashTag = twitterUtils:generate_random_hashtag(),
  lists:append(#twitterClientSimulatorState.prefilledHashtags, HashTag),
  prefill_allowed_hashtags(NumHashTags - 1).


%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #twitterClientSimulatorState{}) ->
  {reply, Reply :: term(), NewState :: #twitterClientSimulatorState{}} |
  {reply, Reply :: term(), NewState :: #twitterClientSimulatorState{}, timeout() | hibernate} |
  {noreply, NewState :: #twitterClientSimulatorState{}} |
  {noreply, NewState :: #twitterClientSimulatorState{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #twitterClientSimulatorState{}} |
  {stop, Reason :: term(), NewState :: #twitterClientSimulatorState{}}).

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #twitterClientSimulatorState{}) ->
  {noreply, NewState :: #twitterClientSimulatorState{}} |
  {noreply, NewState :: #twitterClientSimulatorState{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #twitterClientSimulatorState{}}).
handle_cast(_Request, State = #twitterClientSimulatorState{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #twitterClientSimulatorState{}) ->
  {noreply, NewState :: #twitterClientSimulatorState{}} |
  {noreply, NewState :: #twitterClientSimulatorState{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #twitterClientSimulatorState{}}).
handle_info(_Info, State = #twitterClientSimulatorState{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #twitterClientSimulatorState{}) -> term()).
terminate(_Reason, _State = #twitterClientSimulatorState{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #twitterClientSimulatorState{},
    Extra :: term()) ->
  {ok, NewState :: #twitterClientSimulatorState{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #twitterClientSimulatorState{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================