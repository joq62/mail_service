%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : Simple mail server , main use to send info or alarm from eventhandler
%%% 
%%% Created : 10 dec 2012
%%%
%%%        seq_num(SeqNum) ++ "SELECT " ++ "INBOX"),
%%%        "a15 OK [READ-WRITE] INBOX selected. (Success)"]
%%%
%%%       
%%%        seq_num(SeqNum+1) ++ "SEARCH " ++ "UNSEEN"),
%%%         ["* SEARCH 79",
%%%         "a16 OK SEARCH completed (Success)"]
%%%
%%%
%%%         seq_num(SeqNum+3)  ++ "FETCH " ++ Id ++ " " ++ "BODY.PEEK[HEADER.FIELDS (DATE FROM SUBJECT)]")
%%%         ["* 79 FETCH (BODY[HEADER.FIELDS (DATE FROM SUBJECT)] {107}",
%%%                    "Date: Wed, 6 Mar 2013 23:21:01 +0100",
%%%                    "Subject: set temp 12",
%%%                    "From: Joakim Leche <joakim.leche@gmail.com>",")",
%%%                    "a18 OK Success"]
%%%
%%%         seq_num(SeqNum+4) ++ "FETCH " ++ Id ++ " " ++ "BODY[text]"),

%%%         ["* 79 FETCH (FLAGS (\\Seen) BODY[TEXT] {205}",
%%%                          "--047d7b603fca51bc8b04d74901cb",
%%%                          "Content-Type: text/plain; charset=ISO-8859-1",
%%%                          "--047d7b603fca51bc8b04d74901cb",
%%%                          "Content-Type: text/html; charset=ISO-8859-1",
%%%		"<br>","—047d7b603fca51bc8b04d74901cb--",
%%%              ")"]
%%% 
%%        inför kolla seq numret om inte det stämmer skicka om!

%%% -------------------------------------------------------------------
-module(mail_service).

-behaviour(gen_server).


%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("mail.hrl").

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------
-define(TIMEOUT, 50000).
-define(IMAPDELAY, 60000).
-define(HB_Interval,5*1000).

%% --------------------------------------------------------------------
%% External exports
-export([add_mail/2,delete_mail/2,get_mail_list/0,read_mail/0,
	 connect_send/2,connect_get/2,
	 connect_send/0,connect_get/0,
	 hartbeat/0,
	 disconnect_get/0,disconnect_send/0 ,get_mail/0,send_mail/4,test/0]).

%% gen_server callbacks
-export([start/0,stop/0,
	 init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state,
	{
	  uid_imap,
	  pwd_imap,
	  uid_smtp,
	  pwd_smtp,
	  socket_imap,
	  socket_smtp,
	  seq_num="a0",
	  history=nohistory,
	  mail_list
	}).

%% ====================================================================
%% External functions
%% ====================================================================


start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).


connect_get() ->
    gen_server:call(?MODULE,{connect_imap,?UserId,?PassWd},infinity).
connect_send() ->
    gen_server:call(?MODULE,{connect_smtp,?UserId,?PassWd},infinity).
connect_get(UserId,PassWd) ->
    gen_server:call(?MODULE,{connect_imap,UserId,PassWd},infinity).
connect_send(UserId,PassWd) ->
    gen_server:call(?MODULE,{connect_smtp,UserId,PassWd},infinity).
disconnect_get() ->
    gen_server:call(?MODULE,{disconnect_imap},infinity).
disconnect_send() ->
    gen_server:call(?MODULE,{disconnect_smtp},infinity).
get_mail()->
    gen_server:call(?MODULE,{get_mail},infinity).

send_mail(Subject,Msg,Receiver,Sender) ->
    gen_server:call(?MODULE,{send_mail,Subject,Msg,Receiver,Sender},infinity).    
add_mail(From,SubjectInfo)->
    gen_server:call(?MODULE,{add_mail,From,SubjectInfo},infinity).
delete_mail(From,SubjectInfo)->
    gen_server:call(?MODULE,{delete_mail,From,SubjectInfo},infinity).
get_mail_list()->
        gen_server:call(?MODULE,{get_mail_list},infinity).
test() ->
    gen_server:call(?MODULE,{test},infinity).

hartbeat()->
    gen_server:cast(?MODULE,{hartbeat}).


%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    spawn(fun()->mail_service:hartbeat() end),
    {ok, #state{mail_list=[]}}.

%% --------------------------------------------------------------------
%% Function: connect 
%% Pameters: 
%%       UserId: "userid" : email account ex, "joakim.leche@gmail.com"
%%       PassWd: "passwd" : email account password ex,"qwerty"
%% Description: Starts an ssl session and log on email account
%% Returns: {reply, Reply, State}     
%%      
%% --------------------------------------------------------------------
handle_call({add_mail,From,SubjectInfo}, _From, State) ->
    NewMailList=[{From,SubjectInfo}|State#state.mail_list],
    NewState=State#state{mail_list=NewMailList},
    {reply, ok, NewState};

handle_call({delete_mail,From,SubjectInfo}, _From, State) ->
    NewMailList=lists:delete({From,SubjectInfo},State#state.mail_list),
    NewState=State#state{mail_list=NewMailList},
    {reply, ok, NewState};
    
handle_call({get_mail_list}, _From, State) ->
    Reply=State#state.mail_list,
    {reply, Reply, State};


handle_call({connect_imap,UserId,PassWd}, _From, State) ->
    #state{seq_num=SeqNum}=State,
    application:start(crypto),
    application:start(asn1),
    application:start(public_key),
    application:start(ssl),
    {ok,Socket}=imap_connect(),	    
    recv(Socket),
    send_no_receive(Socket,SeqNum ++" "++ "login " ++ UserId ++ " " ++ PassWd),
    recv(Socket), 
    New_state=State#state{uid_imap=UserId,pwd_imap=PassWd,socket_imap=Socket,seq_num=add_seq_num(SeqNum,1)},
    Reply = ok,
    {reply, Reply, New_state};

handle_call({connect_smtp,UserId,PassWd}, _From, State) ->
    application:start(crypto),
    application:start(asn1),
    application:start(public_key),
    application:start(ssl),
    {ok, Socket} = ssl:connect("smtp.gmail.com", 465, [{active, false}], ?TIMEOUT),
    recv(Socket),
    send(Socket, "HELO localhost"),
    send(Socket, "AUTH LOGIN"),
    _Re_UId = send(Socket, binary_to_list(base64:encode(UserId))), 
    _RE_Passw = send(Socket, binary_to_list(base64:encode(PassWd))),           
    New_state=State#state{uid_smtp=UserId,pwd_smtp=PassWd,socket_smtp=Socket},
    Reply = ok,
    {reply, Reply, New_state};


%% --------------------------------------------------------------------
%% Function: send_mail 
%% Pameters: 
%%       Subject: Subject string in the email , ex "Message from event"
%%       Msg: email body text "Event 1.\n Following actions requires \n"
%%       Receiver: email address to receiver ex,"joakim.leche@gmail.com"
%%       Sender: email of sender ex, "joq.erlang@gmail.com"
%% 
%% Description: Sends one  email
%% Returns: {reply, Reply, State}     
%%      
%% --------------------------------------------------------------------
handle_call({send_mail,Subject,Msg,Receiver,Sender}, _From, State) ->
    #state{socket_smtp=Socket}=State,
    send(Socket, "MAIL FROM:<" ++ Sender ++ ">"),
    send(Socket, "RCPT TO:<" ++ Receiver ++ ">"),
    send(Socket, "DATA"),
    send_no_receive(Socket,"From:<" ++ Sender ++ ">"),
    send_no_receive(Socket,"To:<" ++ Receiver ++ ">"),       
    send_no_receive(Socket, "Date: " ++ date_mail() ++ "Time :" ++ time_mail() ),  
    send_no_receive(Socket, "Subject:" ++ Subject),
    send_no_receive(Socket, ""),
    send_msg(Socket,Msg),
    Reply = send(Socket, "."),
    {reply, Reply,State};
%% --------------------------------------------------------------------
%% Function: get_mail 
%% Pameters: 
%%       Subject: Subject string in the email , ex "Message from event"
%%       Msg: email body text "Event 1.\n Following actions requires \n"
%%       Receiver: email address to receiver ex,"joakim.leche@gmail.com"
%%       Sender: email of sender ex, "joq.erlang@gmail.com"
%% 
%% Description: Sends one  email
%% Returns: {reply, Reply, State}     
%%      
%% --------------------------------------------------------------------

handle_call({get_mail}, _From, State) ->
    #state{socket_imap=Socket,seq_num=SeqNumAcc}=State,

    BoxCmd="SELECT " ++ "INBOX",
    {ok,BoxStr}=get_imap(Socket,SeqNumAcc,BoxCmd),
    [BoxResult|_]=BoxStr,
    BoxR=[SeqNumAcc,"OK","[READ-WRITE]","INBOX","selected.","(Success)"],
    BoxResult = BoxR,

    SeqNumAcc1=add_seq_num(SeqNumAcc,1),
    SearchCmd="SEARCH " ++ "UNSEEN",
    {ok,SearchStr}=get_imap(Socket,SeqNumAcc1,SearchCmd),
 % io:format("Search ~p~n",[SearchStr]),    
    SearchR=[SeqNumAcc1,"OK","SEARCH","completed","(Success)"],
    [SearchResult|TSearch]=SearchStr,
    SearchResult=SearchR,
    [SearchList|_]=TSearch,
    Len = calc_len(SearchList),
    if 
	Len > 2 -> 
% io:format("New mail branch ~n"),
	    NewMail = new_mail,
	    Id =get(mailId,SearchList),	    
	    SeqNumAcc2=add_seq_num(SeqNumAcc1,1),    
	    HeaderCmd="FETCH " ++ Id ++ " " ++ "BODY.PEEK[HEADER.FIELDS (DATE FROM SUBJECT)]",
	    {ok,HeaderStr}=get_imap(Socket,SeqNumAcc2,HeaderCmd),   
% io:format("Header ~p~n",[HeaderStr]),   
	    HeaderR=[SeqNumAcc2,"OK","Success"],
	    [HeaderResult|THeader]=HeaderStr,
	    HeaderResult=HeaderR,
	   {Sender,SubjectInfo}=extract(THeader),

	    SeqNumAcc3=add_seq_num(SeqNumAcc2,1),    
	    TextCmd="FETCH " ++ Id ++ " " ++ "BODY[text]",
	    {ok,TextStr}=get_imap(Socket,SeqNumAcc3,TextCmd),    
	    TextR=[SeqNumAcc3,"OK","Success"],
	    [TextResult|_]=TextStr,
	    TextResult=TextR,
	    SeqAcc=add_seq_num(SeqNumAcc3,1);
	Len == 2 ->
	    SeqAcc=add_seq_num(SeqNumAcc1,1),
	    NewMail = no_new_mail,
	    Sender=null,
	    SubjectInfo=""
    end,
    NewState=State#state{seq_num=SeqAcc},
    Reply = {NewMail,Sender, SubjectInfo},
    {reply, Reply,NewState};    

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_call({disconnect_smtp}, _From, State) ->
    #state{socket_smtp=Socket}=State,
    send(Socket, "QUIT"),
    Reply= ssl:close(Socket),
    application:stop(crypto),
    application:stop(asn1),
    application:stop(public_key),
    application:stop(ssl),   
    {reply, Reply,State};

handle_call({disconnect_imap}, _From, State) ->
    #state{socket_imap=Socket,seq_num=SeqNum}=State, 
    _Send_R= send(Socket,SeqNum ++" "++ "LOGOUT"),
    _CloseR= ssl:close(Socket),
    application:stop(crypto),
    application:stop(asn1),
    application:stop(public_key),
    application:stop(ssl),   
    Reply=ok,
    {reply, Reply,State};
handle_call({stop}, _From, State)->
    terminate("Stuga.erl initiated an exit",State),
     {reply, ok,State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({hartbeat}, State) ->
  %  io:format("hartbeat ~p~n",[{?MODULE,?LINE}]),
    spawn(fun()->hb_local() end),
    {noreply, State};

handle_cast(Msg, State) ->
    io:format("unmatched cast ~p~n",[{?MODULE,?LINE, Msg}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
    io:format("unmatched info ~p~n",[{?MODULE,?LINE,Info}]),
    
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% -------------------------------------------------------------------
hb_local()->
   % io:format("hb_local ~p~n",[{?MODULE,?LINE}]),
    timer:sleep(?HB_Interval),
    MailList= read_mail(),
    case [mail_service:add_mail(From,SubjectInfo)||{From,SubjectInfo}<-MailList] of
	[]->
	    ok;
	[ok]->
	    ok;
	R->
	    io:format("~p~n",[{?MODULE,R}])
    end,
    ok=rpc:call(node(),mail_service,hartbeat,[]),
    ok.   


read_mail()->
    ok=mail_service:connect_get(),
    {R,From,SubjectInfo}=mail_service:get_mail(),
    MailList=rec_mail({R,From,SubjectInfo},[]),
    ok=mail_service:disconnect_get(),  
    MailList.

rec_mail({no_new_mail,_From,_SubjectInfo},MailList)->
    MailList;
rec_mail({new_mail,From,SubjectInfo},Acc) ->
    NewAcc=[{From,SubjectInfo}|Acc],
    {R,From1,SubjectInfo1}=mail_service:get_mail(),
    rec_mail({R,From1,SubjectInfo1},NewAcc).

get(mailId,[_,_|PAR])->    
    [Id|_]=PAR,
    Id.

calc_len([])->
    io:format("Error empty list from FETCH cmd ~n");
calc_len(List)->
    calc_len(List,0).
calc_len([],Len)->
    Len;
calc_len(List,Len)->
    [_H|T]=List,
    Len1 = Len+1,
    calc_len(T,Len1).



send_msg(Socket,Msg)->
    send_no_receive(Socket,Msg). 

send_no_receive(Socket, Data) ->
    ssl:send(Socket, Data ++ "\r\n").

send(Socket, Data) ->
    ssl:send(Socket, Data ++ "\r\n"),
    recv(Socket).

recv(Socket) ->
    case ssl:recv(Socket, 0, ?TIMEOUT) of
	{ok, Return} -> 
						% io:format("~p~n", [Return]),
	    R={ok, Return};
	{error, Reason} -> 
						% io:format("ERROR: ~p~n", [Reason]),
	    R={error, Reason}
    end,
    R.

date_mail()->
    {Y,M,D}=erlang:date(),
    Date = integer_to_list(Y) ++ "-" ++ integer_to_list(M) ++ "-" ++ integer_to_list(D),
    Date.
time_mail()->
    {H,M,S}=erlang:time(),
    Time = integer_to_list(H) ++ "-" ++ integer_to_list(M) ++ "-" ++ integer_to_list(S),
    Time.   

myparser(S)->
    S1=string:tokens(S,"\r\n"),
    S2=myparser(S1,[]), 
    S2.
myparser([],Acc)->
    Acc;
myparser(S,Acc)->
    [H|T]=S,
    Acc1 = [string:tokens(H," ")|Acc],
    myparser(T,Acc1).
%seq_num(SeqNum)->
 %   "a" ++ integer_to_list(SeqNum).

add_seq_num(SeqNum,Int)->
    [N1]=string:tokens(SeqNum,"a"),
    Num=list_to_integer(N1)+Int,
    "a" ++ integer_to_list(Num).    

get_imap(Socket,SeqNum,Cmd)->
    ImapMsg=get_imap(Socket,SeqNum,Cmd,[],retreive),
    {ok,ImapMsg}.

get_imap(Socket,SeqNum,Cmd,Acc,retreive)->
    {ok,ImapMsg}=send(Socket,SeqNum ++ " " ++ Cmd),
%    io:format("ImapMsg ==== ~p~n",[ImapMsg]),
    Z=myparser(ImapMsg),
 %   io:format("ImapMsg parserd ==== ~p~n",[Z]),
    Acc1=lists:append(Z,Acc),
    [Result|_]=Z,
    [SeqNumR|T1]=Result,
    case SeqNumR == SeqNum of
	true->
	    [Ok|_]=T1,
	    case Ok =="OK" of
		true->
		    R=done;
		false->
		    R=retreive
	    end;
	false->
	    R=retreive
    end,
    get_imap(Socket,SeqNum,Cmd,Acc1,R);
	
get_imap(_Socket,_SeqNum,_Cmd,Acc,done)->		    
    Acc.

extract(THeader)->
    Sender=extract(THeader,"From:"),
    SubjectInfo=extract(THeader,"Subject:"),
    {Sender,SubjectInfo}.


extract([],"From:")->
    {error, invalid_sender_address};
extract([HeaderElement|TailElement],"From:")->
    [H|_]=HeaderElement,
    case H of
	"From:"->
	    [Sender|_SenderTail]=string:tokens(lists:last(HeaderElement),"< >"),
	    Sender;
	_ ->
	    Sender=extract(TailElement,"From:")
    end,
    Sender;

extract([],"Subject:")->
    {error, invalid_subject_string};
extract([HeaderElement|TailElement],"Subject:")->
    [H|_]=HeaderElement,
    Reply=case H of
	      "Subject:"->
	    [_|SubjectInfo]=HeaderElement,	 
		  SubjectInfo;   
	      _ ->
		  extract(TailElement,"Subject:")
	  end,
    Reply.
  %  {Cmd,Parameters}.



imap_connect()->
  {Result, Data} = ssl:connect("imap.gmail.com", 993, [{active, false}], ?TIMEOUT),
    case Result of
	ok->
	    R={ok,Data};
	error->
	   io:format("IMAP anslutning misslyckades ~p~n",[Data]),
	   timer:sleep(?IMAPDELAY),
	    R =imap_connect()
    end,
    R.
