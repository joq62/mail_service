%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Created : 7 March 2015
%%% Revsion : 2015-06-19: 1.0.0 :  Created
%%% Description :
%%% Generic tcp server interface to internet and "middle man". Concept 
%%% described in Joe Armstrong book
%%% -------------------------------------------------------------------
-module(mail_test).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

%% --------------------------------------------------------------------
%% Definitions
%% --------------------------------------------------------------------
-define (VERSION,'1.0.0').
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([ver/0]).
%%
%% API Functions
%%

ver()-> {?MODULE,?VERSION}.

%% --------------------------------------------------------------------
%% Function: fun/x
%% Description: fun x skeleton 
%% Returns:ok|error
%% --------------------------------------------------------------------



mail_test()->
    Subject="Test mail",
    Msg="Test mail from mail_test.erl\n Best Regards",
    Receiver="joakim.leche@gmail.com",
    Sender="service.varmdo@gmail.com",
    UserId="service.varmdo@gmail.com",
    PassWd="Festum01",
    {ok,send_mail}=mail:send_mail(Subject,Msg,Receiver,Sender,UserId,PassWd).
    
%%
%% Local Functions
%%
%% --------------------------------------------------------------------
%% Function: fun/x
%% Description: fun x skeleton 
%% Returns:ok|error
%% --------------------------------------------------------------------
