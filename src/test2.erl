%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Created : 7 March 2015
%%% Revsion : 2015-06-19: 1.0.0 :  Created
%%% Description :
%%% Generic tcp server interface to internet and "middle man". Concept 
%%% described in Joe Armstrong book
%%% -------------------------------------------------------------------
-module(test2).

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
-export([start/0]).
%%
%% API Functions
%%

ver()-> {?MODULE,?VERSION}.

%% --------------------------------------------------------------------
%% Function: fun/x
%% Description: fun x skeleton 
%% Returns:ok|error
%% --------------------------------------------------------------------
start()->
    ok=t1_test(),
    ok=t2_test(),
    ok=t2_test(),
    ok=t3_test(),
    init:stop(),
    ok.
t1_test()->
    {ok,_}=mail_service:start(),
    ok.

t2_test()->		 
    Subject="Test mail",
    Msg="Test mail from mail_test.erl\n Best Regards",
    Receiver="joakim.leche@gmail.com",
    Sender="service.varmdo@gmail.com",
    UserId="service.varmdo@gmail.com",
    PassWd="Festum01",
    ?assertEqual(ok,mail_service:connect_send(UserId,PassWd)),
    ?assertMatch({ok,_},mail_service:send_mail(Subject,Msg,Receiver,Sender)),
    ?assertEqual(ok,mail_service:disconnect_send()),    
    ok.
    
t3_test()->
    Subject="cmd1 p1 20",
    Msg="Test mail from mail_test.erl\n Best Regards",
    Receiver="service.varmdo@gmail.com",
    Sender="service.varmdo@gmail.com",
    UserId="service.varmdo@gmail.com",
    PassWd="Festum01",
    ?assertEqual(ok,mail_service:connect_send(UserId,PassWd)),
    ?assertMatch({ok,_},mail_service:send_mail(Subject,Msg,Receiver,Sender)),
    ?assertEqual(ok,mail_service:disconnect_send()),    
  
%  ?assertEqual(ok,mail_service:connect_send(UserId,PassWd)),
%    ?assertMatch({ok,_},mail_service:send_mail(Subject,Msg,Receiver,Sender)),
%    ?assertEqual(ok,mail_service:disconnect_send()),   
 %       timer:sleep(2000),
    ?assertEqual(ok,mail_service:connect_get(UserId,PassWd)),
    ?assertMatch({new_mail,"service.varmdo@gmail.com","cmd1",["p1","20"]},mail_service:get_mail()),
    ?assertEqual(ok,mail_service:disconnect_get()),    
    ok.
    
%%
%% Local Functions
%%
%% --------------------------------------------------------------------
%% Function: fun/x
%% Description: fun x skeleton 
%% Returns:ok|error
%% --------------------------------------------------------------------
