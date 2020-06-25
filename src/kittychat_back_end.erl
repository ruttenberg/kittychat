%%%-------------------------------------------------------------------
%%% @author jpr
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jun 2020 09:42
%%%-------------------------------------------------------------------
-module(kittychat_back_end).
-author("jpr").

-include("kittychat.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([login/1, users/0, peek_messages/1, read_messages/1, send_message/2, lookup_by_username/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(kittychat_back_end_state, {
  users = #{}
}).

%%%===================================================================
%%% API
%%%===================================================================

login(UserName) ->
  Id = gen_server:call(?MODULE, {login, UserName}),
  Id.

users() ->
  Usernames = gen_server:call(?MODULE, usernames),
  Usernames.

peek_messages(Id) ->
  Messages = gen_server:call(?MODULE, {peek_messages, Id}),
  Messages.

read_messages(Id) ->
  Messages = gen_server:call(?MODULE, {read_messages, Id}),
  Messages.

lookup_by_username(Username) ->
  User = gen_server:call(?MODULE, {lookup_by_username, Username}),
  User.

send_message(RecipientName, Message) ->
  gen_server:cast(?MODULE, {send_message, RecipientName, Message}).

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #kittychat_back_end_state{}} | {ok, State :: #kittychat_back_end_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, #kittychat_back_end_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                  State :: #kittychat_back_end_state{}) ->
                   {reply, Reply :: term(), NewState :: #kittychat_back_end_state{}} |
                   {reply, Reply :: term(), NewState :: #kittychat_back_end_state{}, timeout() | hibernate} |
                   {noreply, NewState :: #kittychat_back_end_state{}} |
                   {noreply, NewState :: #kittychat_back_end_state{}, timeout() | hibernate} |
                   {stop, Reason :: term(), Reply :: term(), NewState :: #kittychat_back_end_state{}} |
                   {stop, Reason :: term(), NewState :: #kittychat_back_end_state{}}).
handle_call({login, UserName}, _From, State) ->
  User = kittychat_user:login(UserName),
  #user{id = Id} = User,
  NewState = update_user(User, State),
  {reply, Id, NewState};
handle_call(usernames, _From, State) ->
  Usernames = usernames(State),
  {reply, Usernames, State};
handle_call({peek_messages, UserId}, _From, State) ->
  User = get_user(UserId, State),
  Messages = case User of
               undefined -> [];
               _ -> User#user.messages
             end,
  {reply, Messages, State};
handle_call({read_messages, UserId}, _From, State) ->
  User = get_user(UserId, State),
  Messages = case User of
               undefined -> [];
               _ -> User#user.messages
             end,
  NewState = case User of 
               undefined -> State;
               _ ->
                 UpdatedUser = User#user{messages = []},
                 update_user(UpdatedUser, State)
             end,
  {reply, Messages, NewState};
handle_call({lookup_by_username, UserName}, _From, State) ->
  User = lookup_by_username(UserName, State),
  {reply, User, State};
handle_call(_Request, _From, State = #kittychat_back_end_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #kittychat_back_end_state{}) ->
  {noreply, NewState :: #kittychat_back_end_state{}} |
  {noreply, NewState :: #kittychat_back_end_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #kittychat_back_end_state{}}).
handle_cast({send_message, RecipientName, Message}, State) ->
  User = lookup_by_username(RecipientName, State),
  NewState = case User of
    undefined ->  State;
    _ ->
      #user{messages = Messages} = User,
      NewUser = User#user{messages = Messages ++ [Message]},
      update_user(NewUser, State)
  end, 
  {noreply, NewState};
handle_cast(_Request, State = #kittychat_back_end_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #kittychat_back_end_state{}) ->
  {noreply, NewState :: #kittychat_back_end_state{}} |
  {noreply, NewState :: #kittychat_back_end_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #kittychat_back_end_state{}}).
handle_info(_Info, State = #kittychat_back_end_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                State :: #kittychat_back_end_state{}) -> term()).
terminate(_Reason, _State = #kittychat_back_end_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #kittychat_back_end_state{},
                  Extra :: term()) ->
                   {ok, NewState :: #kittychat_back_end_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #kittychat_back_end_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

update_user(User = #user{id = Id}, State) ->
  NewState = State#kittychat_back_end_state{users = 
                                            maps:put(Id, User, State#kittychat_back_end_state.users)},
  NewState.

usernames(#kittychat_back_end_state{users = Users}) ->
  Ids = maps:keys(Users),
  lists:map(fun(Id) ->
    User = maps:get(Id, Users),
    User#user.name 
            end,
    Ids).
  
get_user(UserId, State) ->
  Users = State#kittychat_back_end_state.users,
  maps:get(UserId, Users, undefined).
  
lookup_by_username(Username, State) ->
  Users = State#kittychat_back_end_state.users,
  UserIds = maps:keys(Users),
  Matches = lists:filter(fun(UserId) ->
    User = maps:get(UserId, Users),
    User#user.name == Username end,
    UserIds),
  case Matches of
    [] -> undefined;
    [Id | _] -> 
      maps:get(Id, Users)
  end.
  
