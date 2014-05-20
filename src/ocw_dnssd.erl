-module(ocw_dnssd).

%% API
-export([start_link/1]).

%% Special process callbacks
-export([init/2,
         system_continue/3,
         system_terminate/4]).

-define(TIMEOUT, 5000).

-define(INITIAL_BACKOFF, 1000).
-define(MAX_BACKOFF, 60*1000).

-record(state, {ref, backoff, service_name}).
%% ===================================================================
%% API
%% ===================================================================
start_link(ServiceName) ->
    proc_lib:start_link(?MODULE, init, [self(), ServiceName]).

%% ===================================================================
%% Special process logic
%% ===================================================================
init(Parent, ServiceName) ->
    register(?MODULE, self()),
    Debug = sys:debug_options([]),
    proc_lib:init_ack(Parent, {ok, self()}),
    InitialState = maybe_register_service(#state{backoff=?INITIAL_BACKOFF,
                                                 service_name=ServiceName}),
    loop(InitialState, Parent, Debug).

loop(#state{ref=Ref}=State, Parent, Debug) ->
    receive
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE, Debug, State);
        {dnssd, Ref, crash} ->            
            State1 = maybe_register_service(State),
            loop(State1, Parent, Debug);
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
maybe_register_service(#state{backoff=Backoff,
                              service_name=ServiceName}=State) ->
    case register_service(ServiceName) of
        {ok, Ref} ->
            State#state{ref=Ref, backoff=?INITIAL_BACKOFF};
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

            
register_service(ServiceName) ->
    {ok, Hostname} = inet:gethostname(),
    NodeName = ServiceName ++ "@" ++ Hostname ++ ".local.",
    case dnssd:register(NodeName, "_" ++ ServiceName ++ "._tcp", 4369) of
        {ok, Ref} ->
            receive
                {dnssd, Ref, {register, _, {RegisteredName, _, _}}} ->
                    RegisteredAtom = binary_to_atom(RegisteredName, utf8),
                    net_kernel:stop(),
                    {ok, _} = net_kernel:start([RegisteredAtom, longnames]),
                    {ok, Ref}
            after ?TIMEOUT ->
                    {error, timeout}
            end;
        Other ->
            Other
    end.
