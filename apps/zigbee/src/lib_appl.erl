%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2024, c50
%%% @doc
%%%
%%% @end
%%% Created :  7 Nov 2024 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(lib_appl).

-include("log.api").
%% API
-export([
	 call/5,
	 test_ok/0,
	 test_no_resource/0,
	 test_badrpc_no_nodes/5,
	 test_crash/5,
	 test_diff_result/5
	 
	]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
call(ResourceType,M,F,A,TO)->
    Result=case [N||{_,N}<-rd:fetch_resources(ResourceType)] of
	       []->
		   ?LOG_WARNING("No resources",[ResourceType]),
		   {error,["No target resources ",ResourceType]};
	       Resources->
		   {ResL,NonExistingNodes}=rpc:multicall(Resources,M,F,A,TO),
		   case NonExistingNodes of
		       []->
			   ok;
		       NonExistingNodes->
			   ?LOG_WARNING("NonExistingNodes",[NonExistingNodes])
		   end,
		   BadRpcList=[{badrpc,Reason}||{badrpc,Reason}<-ResL],
		   case BadRpcList of
		       []->
			   ok;
		       BadRpcList->
			   ?LOG_WARNING("BadRpcList",[BadRpcList])
		   end,
		   case [R||R<-ResL,false=:=lists:member(R,BadRpcList)] of
		       []->
			   {error,["NonExistingNodes and BadRpcList ",NonExistingNodes,BadRpcList]};
		       [Res|T]->
			   case [R||R<-T,Res=/=R] of
			       []->
				   {ok,Res};
			       DiffList ->
				   {error,["Different results",Res,DiffList]}
			   end;
		       X ->
			   {error,["Unmatched signal ",X]}
		   end	   
	   end,
    Result. 

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
 test_diff_result(ResourceType,M,F,A,TO)->
    Result=case [N||{_,N}<-rd:fetch_resources(ResourceType)] of
	       []->
		   ?LOG_WARNING("No resources",[ResourceType]),
		   {error,["No target resources ",ResourceType]};
	       Resources->
		   {ResL,NonExistingNodes}=rpc:multicall(Resources,M,F,A,TO),
		   case NonExistingNodes of
		       []->
			   ok;
		       NonExistingNodes->
			   ?LOG_WARNING("NonExistingNodes",[NonExistingNodes])
		   end,
		   BadRpcList=[{badrpc,Reason}||{badrpc,Reason}<-ResL],
		   case BadRpcList of
		       []->
			   ok;
		       BadRpcList->
			   ?LOG_WARNING("BadRpcList",[BadRpcList])
		   end,
		   case [R||R<-ResL,false=:=lists:member(R,BadRpcList)] of
		       []->
			   {error,["NonExistingNodes and BadRpcList ",NonExistingNodes,BadRpcList]};
		       [Res|T1]->
			   T=[glurk|T1],
			   case [R||R<-T,Res=/=R] of
			       []->
				   {ok,Res};
			       DiffList ->
				   {error,["Different results",Res,DiffList]}
			   end;
		       X ->
			   {error,["Unmatched signal ",X]}
		   end	   
	   end,
    Result. 


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
test_ok()->

    ok. 
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
test_no_resource()->
   ok.

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
test_badrpc_no_nodes(ResourceType,M,F,A,TO)->
    Result=case [N||{_,N}<-rd:fetch_resources(ResourceType)] of
	       []->
		   ?LOG_WARNING("No resources",[ResourceType]),
		   {error,["No target resources ",ResourceType]};
	       Resources1->
		   Resources=[glurk@c50|Resources1],
		   {ResL,NonExistingNodes}=rpc:multicall(Resources,M,F,A,TO),
		   case NonExistingNodes of
		       []->
			   ok;
		       NonExistingNodes->
			   ?LOG_WARNING("NonExistingNodes",[NonExistingNodes])
		   end,
		   BadRpcList=[{badrpc,Reason}||{badrpc,Reason}<-ResL],
		   case BadRpcList of
		       []->
			   ok;
		       BadRpcList->
			   ?LOG_WARNING("BadRpcList",[BadRpcList])
		   end,
		   case [R||R<-ResL,false=:=lists:member(R,BadRpcList)] of
		       []->
			   {error,["NonExistingNodes and BadRpcList ",NonExistingNodes,BadRpcList]};
		       [Res|T]->
			   case [R||R<-T,Res=/=R] of
			       []->
				   {ok,Res};
			       DiffList ->
				   {error,"Different results",[Res,DiffList]}
			   end;
		       X ->
			   {error,["Unmatched signal ",X]}
		   end	   
	   end,
    Result. 


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
test_crash(ResourceType,M,F,A,TO)->
    Result=case [N||{_,N}<-rd:fetch_resources(ResourceType)] of
	       []->
		   _Divi=TO/0,
		   ?LOG_WARNING("No resources",[ResourceType]),
		   {error,["No target resources ",ResourceType]};
	       Resources->
		   {ResL,NonExistingNodes}=rpc:multicall(Resources,M,F,A,TO),
		   case NonExistingNodes of
		       []->
			   ok;
		       NonExistingNodes->
			   ?LOG_WARNING("NonExistingNodes",[NonExistingNodes])
		   end,
		   BadRpcList=[{badrpc,Reason}||{badrpc,Reason}<-ResL],
		   case BadRpcList of
		       []->
			   ok;
		       BadRpcList->
			   ?LOG_WARNING("BadRpcList",[BadRpcList])
		   end,
		   case [R||R<-ResL,false=:=lists:member(R,BadRpcList)] of
		       []->
			   {error,["NonExistingNodes and BadRpcList ",NonExistingNodes,BadRpcList]};
		       [Res|T]->
			   case [R||R<-T,Res=/=R] of
			       []->
				   {ok,Res};
			       DiffList ->
				   {error,"Different results",[Res,DiffList]}
			   end;
		       X ->
			   {error,["Unmatched signal ",X]}
		   end	   
	   end,
    Result. 

%%%===================================================================
%%% Internal functions
%%%===================================================================
