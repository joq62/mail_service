%%% -------------------------------------------------------------------
%%% Author  : joq62
%%% -------------------------------------------------------------------
-module(test3).

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
    ok=t3_test(),
    init:stop(),
    ok.
t1_test()->
    ok=application:start(mail_service),
    ok.
    
t3_test()->
    Receiver="service.varmdo@gmail.com",
    Sender="service.varmdo@gmail.com",
    UserId="service.varmdo@gmail.com",
    PassWd="Festum01",

    Subject1="mfa module1 fun1 arg1",
    Msg1="Glurk",
    ?assertEqual(ok,mail_service:connect_send(UserId,PassWd)),
    ?assertMatch({ok,_},mail_service:send_mail(Subject1,Msg1,Receiver,Sender)),
    ?assertEqual(ok,mail_service:disconnect_send()), 
 %   timer:sleep(1000),
    Subject2="mfa module2 fun2 arg2",
    ?assertEqual(ok,mail_service:connect_send(UserId,PassWd)),
    ?assertMatch({ok,_},mail_service:send_mail(Subject2,Msg1,Receiver,Sender)),
    ?assertEqual(ok,mail_service:disconnect_send()),  
    timer:sleep(12*1000),
              %  [{"service.varmdo@gmail.com","mfa",["module1","fun1","arg1"]},{"service.varmdo@gmail.com","mfa",["module2","fun2","arg2"]}]
    MailList1=mail_service:get_mail_list(),
    ?assertMatch([{"service.varmdo@gmail.com",["mfa","module1","fun1","arg1"]},{"service.varmdo@gmail.com",["mfa","module2","fun2","arg2"]}],MailList1),
    [mail_service:delete_mail(From,SubjectInfo)||{From,SubjectInfo}<-MailList1],
    ?assertEqual([],mail_service:get_mail_list()),
    ok.

t4_test()->
    Receiver="service.varmdo@gmail.com",
    Sender="service.varmdo@gmail.com",
   % UserId="service.varmdo@gmail.com",
   % PassWd="Festum01",

    Subject1="date",
    Msg1="Glurk",
    ?assertEqual(ok,mail_service:connect_send()),
    ?assertMatch({ok,_},mail_service:send_mail(Subject1,Msg1,Receiver,Sender)),
    ?assertEqual(ok,mail_service:disconnect_send()), 
    timer:sleep(12*1000),
    [{From,SubjectInfo}]=mail_service:get_mail_list(),
  %  {Y,M,D}=rpc:call(node(),list_to_atom(MStr),list_to_atom(FStr),[]),
   % Subject2="{"++integer_to_list(Y)++","++integer_to_list(M)++","++integer_to_list(D)++"}",
    ?assertEqual(ok,mail_service:connect_send()),
   % ?assertMatch({ok,_},mail_service:send_mail(Subject2,Msg1,Receiver,Sender)),
    ?assertEqual(ok,mail_service:disconnect_send()), 
    ok.
 

%%
%% Local Functions
%%
%% --------------------------------------------------------------------
%% Function: fun/x
%% Description: fun x skeleton 
%% Returns:ok|error
%% --------------------------------------------------------------------
