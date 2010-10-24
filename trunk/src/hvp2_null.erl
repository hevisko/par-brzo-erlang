%% Author: hvisage
%% Created: Jun 18, 2010
%% Description: TODO: Add description to hvp1
-module(hvp2_null).
%-import(hvt,[reduce/1,deriv/2]).
-copyright("Hendrik Visage 2010").

-vsn('$Revision: 1.3 $').
%$Id: hvp2_null.erl,v 1.3 2010/06/25 19:12:54 hendrivi Exp hendrivi $

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("mds.hrl").

-export([hv_brz/4]).

%% ====================================================================!
%% External functions
%% ====================================================================!

%The entry to hvp2
% third Parameter to chose roundrobin or available
hv_brz(RE, Sigma, available, N) ->
    TimeOut = 3000,
    Res = self(),
    Dist = spawn(fun () -> hv_dist_avail_start(TimeOut, Res, N)
		 end),
    WiP = [],%Only the first (and last) one would be "empty"
    Finish = [],
    Dlist = [RE],%Start state.
    Delta = dict:new(),
    hv_brzp_null(RE, Sigma, Dist, WiP, Finish, Dlist, Delta);
hv_brz(RE, Sigma, roundrobin, N) ->
    TimeOut = 3000,
    Res = self(),
    Dist = spawn(fun () -> hv_dist_rr_start(TimeOut, Res, N) end),
    WiP = [],%Only the first (and last) one would be "empty"
    Finish = [],
    Dlist = [RE],%Start state.
    Delta = dict:new(),
    hv_brzp_null(RE, Sigma, Dist, WiP, Finish, Dlist, Delta).

%Specialized one with only two using the RoundRobin method 
hv_brz(RE,Sigma,rr_spec) ->
	TimeOut=3000,
	Res=self(),
	Dist=spawn(fun() -> hv_dist_rr_spec_start(TimeOut,Res) end),
	WiP=[],%Only the first (and last) one would be "empty"
	Finish=[],
	Dlist=[RE],%Start state.
	Delta=dict:new(),
	hv_brzp_null(RE,Sigma,Dist,WiP,Finish,Dlist,Delta).

%% ====================================================================
%% Internal functions
%% ====================================================================


%For all the Sigma add {E,i} to the Work In Progress
add_wip(WiP, Type, E, [H]) -> [{Type, E, H}| WiP];
add_wip(WiP, Type, E, [H| SigmaT]) -> add_wip([{Type, E, H}| WiP], Type, E, SigmaT).

%The receiver for the round robin/sequential tests
%
hv_rr_rec(Name,Res) -> 
	%io:format(" starting: ~p self:~p REs:~p~n",[Name,pid_to_list(self()),pid_to_list(Res)]),
	receive 
		{stop} -> %io:format("stopping ~p ~n",[Name]),
				  false;
		{process,[rd,E,I]} -> Res!{rd,E,I,mds:reduce(mds:deriv(E,I))},
						 hv_rr_rec(Name,Res);
		{process,[null,E]} -> Res!{null,E,mds:null(E)},
													 hv_rr_rec(Name,Res);
		Other -> io:write(Other),throw(Other)
		after 3000 ->	io:format("Timeout ~p quiting",[Name]),io:nl()
	end.

%Start N round-robin receivers that will send their results to Res
% returning the list of  PIDs.
list_start_servers(0,_Res) -> [];
list_start_servers(N,Res) -> 
	[spawn(fun()->hv_rr_rec("Receiver "++[N+$0],Res) end)|list_start_servers(N-1,Res)].

%Number of servers variable, should make that a number to pass too, but 
% for the moment this is adequate to test etc.
hv_dist_rr_start(TimeOut,Res,N) -> 
	hv_dist_rr(list_start_servers(N,Res),TimeOut).

%Two specified servers
hv_dist_rr_spec_start(TimeOut,Res)->
	Rec1=spawn(fun() ->hv_rr_rec("Rec1",Res) end),receive after 100 -> true end,
	Rec2=spawn(fun() ->hv_rr_rec("Rec2",Res) end),receive after 100 -> true end,
	hv_dist_rr([Rec1,Rec2],TimeOut).

%Round Robin distributor.. we know this is not "optimal" :)
hv_dist_rr([H|T]=Receivers,TimeOut) ->
	%io:format("Dist_rr starting: SendTo: ~p Self:~p ~n",[pid_to_list(H),pid_to_list(self())]),
	receive
		{stop} -> lists:foreach(fun(X)->X!{stop} end,Receivers);
		{process,Param} -> H!{process,Param},hv_dist_rr(lists:append(T, [H]),TimeOut);
		Other -> io:write(Other),throw(Other)
		after TimeOut ->
			io:format("Dist quiting and stopping receivers"),
			lists:foreach(fun(X)->X!{stop} end,Receivers)
	end.


output_mailbox(N) ->
	receive
		Mess -> io:format("Message ~p~n:",[N]),io:write(Mess),io:nl(),output_mailbox(N+1)
  after 0 -> exit(123)
	end.

%The case when the WiP is empty
hv_brzp_null(receive_only,Sigma,Dist,[],Finish,Dlist,Delta) ->
	io:format("WiP finished"), 
	Dist!{stop},
	#dfa{states=lists:sort(Dlist),symbols=Sigma,start=lists:last(Dlist),transition=Delta,finals=Finish};


%Receive only, nothing to derive
hv_brzp_null(receive_only,Sigma,Dist,WiP,Finish,Dlist,Delta) ->
	%io:format("Receive only ~n"),
	receive
		{rd,E,I,DDI} -> %io:format("brzp_null_2:"),io:write({rd,E,I,DDI}),io:format("~n"),
			NewDelta=dict:store({E,I},DDI,Delta),
			case lists:member(DDI,Dlist) of
				true -> hv_brzp_null(receive_only,Sigma,Dist,lists:delete({rd,E,I},WiP),Finish,Dlist,NewDelta);
				false -> hv_brzp_null(DDI,Sigma,Dist,lists:delete({rd,E,I},WiP),Finish,[DDI|Dlist],NewDelta)
			end;
		{null,E,true} -> %io:format("brzp_null_2: ~p true~n",[E]),
			hv_brzp_null(receive_only,Sigma,Dist,lists:delete({null,E},WiP),[E|Finish],Dlist,Delta);	% Add nullable states to F
		{null,E,false} ->%io:format("brzp_null+2: ~p false~n",[E]),
			hv_brzp_null(receive_only,Sigma,Dist,lists:delete({null,E},WiP),Finish,Dlist,Delta)
		%Other -> io:write(Other),throw({2,Other})
		after 5000 ->	io:write(WiP),output_mailbox(1),throw(timeoutRec_only)
	end;


% When we have an RE/E d/di that needs to be derived/etc.
hv_brzp_null(E,Sigma,Dist,WiP,Finish,Dlist,Delta) ->
	%io:format("hv_brzp2  ~p ~n",[lists:flatten(io_lib:write(E,5))]),
	%foreach(Sigma) send message to Dist
	lists:foreach(fun(X) -> Dist!{process,[rd,E,X]} end,Sigma),
	%F1=nullable(E,Finish),
	%nullable(RE),
	Dist!{process,[null,E]},
	%foreach(Sigma) insert {E,I} into WiP, and add the null to the begining ;)
	NewWiP=[{null,E}|add_wip(WiP,rd,E,Sigma)],
	
	%WiP would not be empty in this function :)
	receive
		{rd,E,I,DDI} -> %io:format("brzp_null_why:  "),io:write({rd,E,I,DDI}),io:format("~n"),%"~p ~p ~p~n",[E,I,DDI]),
			NewDelta=dict:store({E,I},DDI,Delta),
			case lists:member(DDI,Dlist)  of
				true -> hv_brzp_null(receive_only,Sigma,Dist,lists:delete({rd,E,I},NewWiP),Finish,Dlist,NewDelta);
				false -> hv_brzp_null(DDI,Sigma,Dist,lists:delete({rd,E,I},NewWiP),Finish,[DDI|Dlist],NewDelta)
			end;
		{null,E,true} -> % io:format("brzp_null: ~p true~n",[E]),
						  hv_brzp_null(receive_only,Sigma,Dist,lists:delete({null,E},NewWiP),[E|Finish],Dlist,Delta);	% Add nullable states to Finish
		{null,E,false} -> %io:format("brzp_null: ~p false~n",[E]),
			hv_brzp_null(receive_only,Sigma,Dist,lists:delete({null,E},NewWiP),Finish,Dlist,Delta)
		%Other -> throw (Other)
		after 5000 ->	io:write(WiP),throw(timeOut)
end.


%===============================================
%The distributor that  have receivers that tells it when 
% they are finished and ready for new processing
%===============================================

%Let the Distributor know when finished with processing
% But this state engine not rock solid when more than one messages was sent 
% etc.
hv_rec_available(Timeout,Name,Res,Dist) ->
	%io:format("~nEntering ~p ~p ~p ~p ~n",[Name,integer_to_list(Timeout),pid_to_list(Res),pid_to_list(Dist)]),
	%First we handle all stop/process messages on the queue
	%If the distributor works correct, this shouldn't be necesssary,
	%But we could "modify" the distributor to send expected "short"
	% requests in rapid fire??
	receive
		{stop} ->% io:format("stopping ~p~n",[Name]),
				  exit(0); %Need to do the exit here else continue to next
		{process,[rd,E1,I1]}-> %io:format("~p ~p ~p~n",[Name,E1,I1]),
			Res!{rd,E1,I1,mds:reduce(mds:deriv(E1,I1))},
			hv_rec_available(Timeout,Name,Res,Dist);
		{process,[null,E1]} -> %io:format("Null: ~p ~p ~n",[Name,E1]),
			Res!{null,E1,mds:null(E1)},
			hv_rec_available(Timeout,Name,Res,Dist);
		Other1 -> throw(Other1)
		after 0 -> Dist!{available,self()} %Nothing in queue, so we let the Distributor know
	end,
	receive %There were no "normal"/"expected" messages in the queue, so lets wait :)
		{stop} -> %io:format("stopping ~p~n",[Name]),
				  true;
		{process,[rd,E,I]} -> %io:format("~p ~p ~p~n",[Name,E,I]),
			Res!{rd,E,I,mds:reduce(mds:deriv(E,I))},
			hv_rec_available(Timeout,Name,Res,Dist);
		{process,[null,E]} -> %io:format("Null: ~p ~p ~n",[Name,E]),
			Res!{null,E,mds:null(E)},
			hv_rec_available(Timeout,Name,Res,Dist);
		Other -> throw(Other)
		after Timeout ->
			io:format("Timeout ~p quiting ~n",[Name])
	end.


%The Available distributor
%First the "empty" case
hv_dist_available(Timeout,[]) ->
	%io:format("Entering dist_available []~n"),
	receive
		{available,PID}->
			receive %We check for the availability of a process message to "fast track", else call the normal wait
				{process,Param}->PID!{process,Param},hv_dist_available(Timeout,[]) %Goody! a message available
				after 0-> hv_dist_available(Timeout,[PID]) %Normal wait since no process message in mailbox
			end;
		{stop} -> io:format("Distributor stopping from empty state~n") %no available receivers to stop :(
		after Timeout ->
			io:format("timeout distributor from waiting state~n")
	end;
hv_dist_available(Timeout,[H|Tail])-> %At least have a PID to check
	%io:format("Entering dist_available with~n"),
	receive
		{available,PID}->hv_dist_available(Timeout,[H,PID|Tail]); %H or PID, shouldn't matter which is first'
		{process,Param}->H!{process,Param},
					   hv_dist_available(Timeout,Tail);
		{stop}-> lists:foreach(fun(X)->X!{stop} end,[H|Tail]); %Stop all the available receivers
		Other -> throw(Other)
		after Timeout ->
			io:format("Timeout distributor fron available state~n")
	end.

%Start the receivers and the distributor
hv_dist_avail_start(Timeout,_Res,0)	->
		io:format("Empty~n"),
		hv_dist_available(Timeout,[]);
hv_dist_avail_start(Timeout,Res,N) when N>0 ->
	%io:format("~n~p: Dist avail ~p ~p ~p~n~n",
	%		  [pid_to_list(self()),erlang:integer_to_list(Timeout),
	%		   erlang:pid_to_list(Res),erlang:integer_to_list(N)]),
	Dist=self(),
	spawn(fun() -> hv_rec_available(Timeout,"Receiver "++erlang:integer_to_list(N),Res,Dist) end),
	hv_dist_avail_start(Timeout,Res,N-1).




%% $Log: hvp2_null.erl,v $
%% Revision 1.3  2010/06/25 19:12:54  hendrivi
%% some I have missed: The states=lists:sort(D) is added to make the DFAs equivalent especially after the mds:convert_dfa/1
%%
%% Revision 1.2  2010/06/21 15:44:56  hendrivi
%% Removed nullable(E,F) as it's used differently inside these loops
%%
%% Revision 1.1  2010/06/21 15:40:46  hendrivi
%% Initial revision
%%
%%
%% copied and renamed to hvp2_null from hvp1
%%-----------------------------------------------------------
%%
%% Revision 1.13  2010/06/20 18:04:22  hvisage
%% some more cleaning up a bit of the code and comments etc.
%%
%% Revision 1.12  2010/06/20 18:01:42  hvisage
%% cleaning up a bit of the code and comments etc.
%%
%% Revision 1.11  2010/06/20 15:44:03  hvisage
%% Export
%%
%% Revision 1.10  2010/06/20 15:06:28  hvisage
%% Available receiver distributor
%%
%% Revision 1.9  2010/06/19 22:21:01  hvisage
%% Added receiver available code
%%
%% Revision 1.8  2010/06/19 21:55:52  hvisage
%% Debugged... :)
%%
%% Revision 1.7  2010/06/19 21:20:25  hvisage
%% debugging etc.
%%
%% Revision 1.6  2010/06/19 18:59:25  hvisage
%% implemented??
%%
%% Revision 1.5  2010/06/19 17:42:33  hvisage
%% With some keywords
%%
%% Revision 1.4  2010/06/19 17:40:50  hvisage
%% With some keywords
%%
%% Revision 1.3  2010/06/19 17:39:34  hvisage
%% With some keywords
%%