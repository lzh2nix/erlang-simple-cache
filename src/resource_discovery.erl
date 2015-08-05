-module(resource_discovery).
-behaviour(gen_server).
-export([start_link/0,
         add_target_resource_type/1,
         add_local_resource/2,
         fetch_resource/1,
         trade_resource/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {target_resource_types,
                local_resource_tuples,
                found_resource_tuples}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, #state{target_resource_types = [],
                local_resource_tuples = dict:new(),
                found_resource_tuples = dict:new()}}.
add_target_resource_type(Type) ->
    gen_server:cast(?SERVER, {add_target_resource_type, Type}).
add_local_resource(Type, Instance) ->
    gen_server:cast(?SERVER, {add_local_resource, {Type, Instance}}).

handle_cast({add_target_resource_type, Type}, State) ->
    TargeTypes = State#state.target_resource_types,
    NewTargetTypes = [Type, | lists:delete(Type, TargeTypes)],
    {noreply, State#state.target_resource_types = NewTargetTypes}.

handle_cast({add_local_resource, {Type, Instance}}, State) ->
    ResourceTuples = State#state.local_resource_tuples,
    NewResourceTuples = add_resource(Type, Instance, ResourceTuples),
    {noreply, State#state.local_resource_tuples = NewResourceTuples}.

add_resource(Type, Resource, ResourceTuples) ->
    case  dict:find(Type, ResourceTuples) of
        {ok, ResourceList} ->
            NewList = [Resource | lists:delete(Resource, ResourceTuples)],
            dict:store(Type, NewList, ResourceTuples);
        error ->
            dict:store(Type, NewList, ResourceTuples)
    end.

fetch_resource(Type) ->
    gen_server:call(?SERVER, {fetch_resource, Type}).

handle_call({fetch_resource, Type}, _, State) ->
    {reply, dict:find(Type, State#state.found_resource_tuples), State};
trade_resource() ->
    gen_server:cast(?SERVER, trade_resource).
handle_cast(trade_resource, State) ->
    ResourceTuples = State#state.local_resource_tuples,
    AllNodes = [node() | nodes()],
    lists:foreach(
      fun(Node) ->
              gen_server:cast({?SERVER, Node}, {trade_resource, {node(), ResourceTuples}})
      end, AllNodes),
    {noreply, State};
handle_cast() ->
