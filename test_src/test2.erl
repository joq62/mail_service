%%% -------------------------------------------------------------------
%%% Author  : joq62
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
    ok=t4_test(),
    init:stop(),
    ok.
t1_test()->
    ok=application:start(mail_service),
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
    Subject="fun erlang date []",
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
    {new_mail,From,Cmd,[M,F,A]}=mail_service:get_mail(),
    ?assertMatch({new_mail,"service.varmdo@gmail.com","fun",["erlang","date","[]"]},{new_mail,From,Cmd,[M,F,A]}),
    case A of
	"[]"->
	    D=date(),
	    ?assertEqual(D,apply(list_to_atom(M),list_to_atom(F),[]));
	_->
	    ok
    end,
    ?assertEqual(ok,mail_service:disconnect_get()),    
    ok.
 

t4_test()->
    Subject="mfa tellstick_service mail varme_pa",
    Msg="Glurk",
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

    {new_mail,From,Cmd,[M,F,A]}=mail_service:get_mail(),
    ?assertMatch({new_mail,"service.varmdo@gmail.com","mfa",["tellstick_service","mail","varme_pa"]},{new_mail,From,Cmd,[M,F,A]}),
    case A of
	"[]"->
	    D=date(),
	    ?assertEqual(D,apply(list_to_atom(M),list_to_atom(F),[]));
	_->
	    ok
    end,
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
