-module(ocw_dnssd).

%% API
-export([start_link/1,
         start_link/2,
         start_link/3,
         start_link/4]).

%% Special process callbacks
-export([init/5,
         system_continue/3,
         system_terminate/4]).

-define(TIMEOUT, 5000).

-define(INITIAL_BACKOFF, 1000).
-define(MAX_BACKOFF, 60*1000).

-define(PORT, 4369).

-define(IDENTITY, fun(X) -> X end).

-record(state, {register_ref,
                browse_ref,
                browse_nodes,
                backoff,
                node_type,
                register_fun,
                result_fun}).
%% ===================================================================
%% API
%% ===================================================================
start_link(NodeType) ->
    start_link(NodeType, [], ?IDENTITY).

start_link(NodeType, RegisterFun) ->
    start_link(NodeType, RegisterFun, [], ?IDENTITY).

start_link(NodeType, BrowseNodes, ResultFun) ->
    start_link(NodeType, ?IDENTITY, BrowseNodes, ResultFun).

start_link(NodeType, RegisterFun, BrowseNodes, ResultFun) ->
    proc_lib:start_link(?MODULE, init,
                        [self(),NodeType,RegisterFun,BrowseNodes,ResultFun]).

%% ===================================================================
%% Special process logic
%% ===================================================================
init(Parent, NodeType, RegisterFun, BrowseNodes, ResultFun) ->
    register(?MODULE, self()),
    Debug = sys:debug_options([]),
    proc_lib:init_ack(Parent, {ok, self()}),
    State1 = #state{backoff = ?INITIAL_BACKOFF,
                    node_type = NodeType,
                    result_fun = ResultFun,
                    register_fun = RegisterFun,
                    browse_nodes = BrowseNodes,
                    browse_ref = dict:new()},
    State2 = maybe_register_service(State1),
    loop(State2, Parent, Debug).

loop(#state{register_ref=RegRef,browse_ref=BrowseRef,result_fun=Fun}=State,
     Parent, Debug) ->
    receive
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE, Debug, State);
        {dnssd, RegRef, crash} ->            
            State1 = maybe_register_service(State),
            loop(State1, Parent, Debug);
        {dnssd, Ref, {browse, Action, {NodeName, Service, _}}} ->
            NodeType = node_type(Service),
            NodeNameA = binary_to_atom(NodeName, utf8),
            case dict:find(NodeType, BrowseRef) of
                {ok, Ref} -> Fun({Action, NodeType, NodeNameA});
                _         -> ok
            end,
            loop(State, Parent, Debug);
        backoff ->
            State1 = maybe_register_service(State),
            loop(State1, Parent, Debug);
        _ ->
            loop(State, Parent, Debug)
    end.

system_continue(Parent, Debug, State) ->
    loop(State, Parent, Debug).

system_terminate(Reason, _Parent, _Debug, _State) ->
    exit(Reason).

%% ===================================================================
%% Private functions
%% ===================================================================
browse_nodes([], State) ->
    State;
browse_nodes([Node|Rest], #state{browse_ref=BrowseRef}=State) ->
    {ok, Ref} = dnssd:browse(service_name(Node)),
    BrowseRef1 = dict:store(Node, Ref, BrowseRef),
    browse_nodes(Rest, State#state{browse_ref=BrowseRef1}).

maybe_register_service(#state{backoff=Backoff,
                              node_type=NodeType,
                              browse_nodes=BrowseNodes,
                              register_fun=RegisterFun}=State) ->
    case register_service(NodeType, RegisterFun) of
        {ok, Ref} ->
            State1 = State#state{register_ref=Ref, backoff=?INITIAL_BACKOFF},
            browse_nodes(BrowseNodes, State1);
        _ ->
            erlang:send_after(Backoff, self(), backoff),
            NewBackoff = case Backoff*2 of
                Greater when Greater > ?MAX_BACKOFF ->
                    ?MAX_BACKOFF;
                Lower ->
                    Lower
            end,
            State#state{backoff=NewBackoff}
    end.

            
register_service(Node, RegisterFun) ->
    {ok, Hostname} = inet:gethostname(),
    ServiceName = service_name(Node),
    case dnssd:register(node_name(Node, Hostname), ServiceName, ?PORT) of
        {ok, Ref} ->
            receive
                {dnssd, Ref, {register, _, {RegisteredName, _, _}}} ->
                    RegisteredAtom = binary_to_atom(RegisteredName, utf8),
                    net_kernel:stop(),
                    {ok, _} = net_kernel:start([RegisteredAtom, longnames]),
                    RegisterFun(RegisteredAtom),
                    {ok, Ref}
            after ?TIMEOUT ->
                    {error, timeout}
            end;
        Other ->
            Other
    end.

node_type(Service) ->
    [NodeTypeL|_] = re:split(Service, "._tcp", [{return, list}]),
    [$_|Rest] = NodeTypeL,
    list_to_atom(Rest).

service_name(Name) ->
    "_" ++ atom_to_list(Name) ++ "._tcp".

node_name(Service, Host) ->
    atom_to_list(Service) ++ "@" ++ Host ++ ".local.".
