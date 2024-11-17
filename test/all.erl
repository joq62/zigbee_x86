%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% Created :
%%%
%%% -------------------------------------------------------------------
-module(all).       
 
-export([start/0]).


%%
-define(CheckDelay,20).
-define(NumCheck,1000).


%% Change
-define(Appl,"zigbee").
-define(Dir,"zigbee").
-define(ApplAtom,list_to_atom(?Appl)).

-define(NodeName,?Appl).
-define(ApplDir,?Dir++"_container").
-define(TarFile,?Appl++".tar.gz").
-define(TarDir,"tar_dir").
-define(ExecDir,"exec_dir").
-define(GitUrl,"https://github.com/joq62/"++?Appl++"_x86.git ").

-define(Foreground,"./"++?ApplDir++"bin/"++?Appl++" "++"foreground").
-define(Daemon,"./"++?ApplDir++"/bin/"++?Appl++" "++"daemon").

-define(LogFile,?Appl++"_container/logs/"++?Appl++"/log.logs/test_logfile.1").


%%
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
   
    ok=setup(),
    ApplicationToTest=list_to_atom("test_"++?Appl),
    ok=rpc:call(get_node(?NodeName),ApplicationToTest,start,[],10*5000),

  %  file:del_dir_r(?ApplDir),   
  %  rpc:call(get_node(?NodeName),init,stop,[],5000),
  %  true=check_node_stopped(get_node(?NodeName)),
    io:format("Test OK !!! ~p~n",[?MODULE]),
    log_loop([]),

  %  timer:sleep(2000),
  %  init:stop(),
    ok.





%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
log_loop(Strings)->    
    Info=os:cmd("cat "++?LogFile),
    NewStrings=string:lexemes(Info,"\n"),
    
    [io:format("~p~n",[String])||String<-NewStrings,
				 false=:=lists:member(String,Strings)],
    timer:sleep(10*1000),
    log_loop(NewStrings).
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
   
    file:del_dir_r(?ApplDir),
    file:make_dir(?ApplDir),
   
    %% Unpack tar file
    TarFileFullPath=filename:join([?TarDir,?TarFile]),
    os:cmd("tar -zxvf "++TarFileFullPath++" "++"-C"++" "++?ApplDir),
  
    rpc:call(get_node(?NodeName),init,stop,[],5000),
    true=check_node_stopped(get_node(?NodeName)),
    io:format("~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    %% Start application to test and check node started
    []=os:cmd(?Daemon),
    true=check_node_started(get_node(?NodeName)),
    
    %% Check applications are correct started
    pong=rpc:call(get_node(?NodeName),log,ping,[],5000),
    pong=rpc:call(get_node(?NodeName),rd,ping,[],5000),

    %% Change
    pong=rpc:call(get_node(?NodeName),zigbee_server,ping,[],3*5000),
    pong=rpc:call(get_node(?NodeName),?ApplAtom,ping,[],3*5000),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------

check_node_started(Node)->
    check_node_started(Node,?NumCheck,?CheckDelay,false).

check_node_started(_Node,_NumCheck,_CheckDelay,true)->
    true;
check_node_started(_Node,0,_CheckDelay,Boolean)->
    Boolean;
check_node_started(Node,NumCheck,CheckDelay,false)->
    case net_adm:ping(Node) of
	pong->
	    N=NumCheck,
	    Boolean=true;
	pang ->
	    timer:sleep(CheckDelay),
	    N=NumCheck-1,
	    Boolean=false
    end,
 %   io:format("NumCheck ~p~n",[{NumCheck,?MODULE,?LINE,?FUNCTION_NAME}]),
    check_node_started(Node,N,CheckDelay,Boolean).
    
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------

check_node_stopped(Node)->
    check_node_stopped(Node,?NumCheck,?CheckDelay,false).

check_node_stopped(_Node,_NumCheck,_CheckDelay,true)->
    true;
check_node_stopped(_Node,0,_CheckDelay,Boolean)->
    Boolean;
check_node_stopped(Node,NumCheck,CheckDelay,false)->
    case net_adm:ping(Node) of
	pang->
	    N=NumCheck,
	    Boolean=true;
	pong ->
	    timer:sleep(CheckDelay),
	    N=NumCheck-1,
	    Boolean=false
    end,
 %   io:format("NumCheck ~p~n",[{NumCheck,?MODULE,?LINE,?FUNCTION_NAME}]),
    check_node_stopped(Node,N,CheckDelay,Boolean).    
    

get_node(NodeName)->
    {ok,Host}=net:gethostname(),
    list_to_atom(NodeName++"@"++Host).
