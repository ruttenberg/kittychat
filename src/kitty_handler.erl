%%%-------------------------------------------------------------------
%%% @author jpr
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Jun 2020 19:15
%%%-------------------------------------------------------------------
-module(kitty_handler).
-author("jpr").

%% API
-behavior(cowboy_handler).

-export([init/2, fold_csv/1]).

init(Req0, State) ->
  Path       = cowboy_req:binding(path, Req0),
  Req = handle_path(Path, Req0),
  {ok, Req, State}.

reply_with_content(Content, Req0) ->
  cowboy_req:reply(200,
                   #{<<"content-type">> => <<"text/plain">>},
                   Content,
                   Req0).

handle_path(undefined, Req0)  ->
  reply_with_content(<<"Hello Kitty!">>, Req0);
handle_path(<<"login">>, Req0)  ->
  login(Req0);
handle_path(<<"messages-for-me">>, Req0)  ->
  messages_for_me(Req0);
handle_path(<<"who">>, Req0)  ->
  who(Req0);
handle_path(<<"message">>, Req0)  ->
  message(Req0);
handle_path(<<"wall">>, Req0)  ->
  wall(Req0);
handle_path(<<"logoff">>, Req0)  ->
  logoff(Req0);
handle_path(Path, Req0)  ->
  reply_with_content(<< <<"unknown command: ">>/binary, Path/binary >>, Req0).

reply_with_not_found(Req0) ->
  cowboy_req:reply(404, Req0).
  
found_parameter(ParameterName, Req0) ->
  QsVals = cowboy_req:parse_qs(Req0),
  Value = proplists:get_value(ParameterName,QsVals),
  case Value of
    undefined -> undefined;
    _ -> Value
  end.

found_parameters(ParameterNames, Req0) ->
  Values = lists:map(fun(ParameterName)-> found_parameter(ParameterName, Req0) end, ParameterNames),
  case lists:member(undefined, Values) of
    true -> undefined;
    false -> Values
  end.
  
valid_id(Id) ->
  Id,
  true.

login(Req0) ->
  case found_parameter(<<"name">>, Req0) of
    undefined -> reply_with_not_found(Req0);
    Name -> 
      Id = kittychat_back_end:login(Name),
      reply_with_content(http_uri:encode(Id), Req0)
  end.
  
messages_for_me(Req0) ->
  case found_parameter(<<"id">>, Req0) of
    undefined -> reply_with_not_found(Req0);
    Id -> case valid_id(Id) of
            false -> reply_with_not_found(Req0);
            true -> 
              User = kittychat_user:username_from(Id),
              Messages = kittychat_back_end:read_messages(Id),
              MessagePerLine = message_per_line(Messages),
              reply_with_content(<<
                <<"All messages for ">>/binary, 
                User/binary,
                <<"\n">>/binary,
                MessagePerLine/binary 
                                 >>, Req0)
          end
  end.

who(Req0) ->
  case found_parameter(<<"id">>, Req0) of
    undefined -> reply_with_not_found(Req0);
    Id -> case valid_id(Id) of
            false -> reply_with_not_found(Req0);
            true -> 
              Usernames = kittychat_back_end:users(),
              reply_with_content(fold_csv(Usernames), Req0)
          end
  end.

message(Req0) ->
  case found_parameters([<<"id">>, <<"recipient">>, <<"message">>], Req0) of
    undefined -> reply_with_not_found(Req0);
    [Id, Recipient, Message] ->
      Name= kittychat_user:username_from(Id),
      kittychat_back_end:send_message(Recipient, Message),
      reply_with_content(<<
                           <<"message from ">>/binary,
                           Name/binary, 
                           <<" to ">>/binary,
                           Recipient/binary, 
                           <<": ">>/binary,
                           Message/binary
                           >>, Req0)
  end.
  
wall(Req0) ->
  case found_parameters([<<"id">>, <<"message">>], Req0) of
    undefined -> reply_with_not_found(Req0);
    [Id, Message] ->
      case valid_id(Id) of
        false -> reply_with_not_found(Req0);
        true ->
          Start = <<"sent to everybody: ">>,
          reply_with_content(<<
                               Start/binary,
                               Message/binary
                             >>, Req0)
      end
  end.

logoff(Req0) ->
  case found_parameter(<<"id">>, Req0) of
    undefined -> reply_with_not_found(Req0);
    Id ->
      User = kittychat_user:username_from(Id),
      reply_with_content(<< 
                           <<"logged off user ">>/binary, 
                           User/binary
                         >>, Req0)
  end.

fold_csv(Binaries) ->
  lists:foldl(fun(Item, Current) ->
    case Current of
      <<>> -> Item;
      _ -> <<
        Current/binary,
        <<", ">>/binary,
        Item/binary
      >>
    end
              end, <<>>, Binaries).

message_per_line(Messages) ->
  MessageBinary = lists:foldl(fun(Message, Current) ->
    case Current of
      <<>> -> Message;
      _ -> <<
        Current/binary,
        <<"\n">>/binary,
        Message/binary
      >>
    end
              end, <<>>, Messages),
  << MessageBinary/binary, <<"\n">>/binary >>.
