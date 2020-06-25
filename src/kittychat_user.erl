%%%-------------------------------------------------------------------
%%% @author jpr
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jun 2020 09:58
%%%-------------------------------------------------------------------
-module(kittychat_user).
-author("jpr").

-include("kittychat.hrl").

%% API
-export([login/1, opaque_name/1, username_from/1]).

opaque_name(Name) ->
  base64:encode(term_to_binary(Name)).

username_from(Id) ->
  binary_to_term(base64:decode(http_uri:decode(Id))).

login(Name) ->
  #user{name = Name,
        id = opaque_name(Name),
        messages = [] 
  }.
