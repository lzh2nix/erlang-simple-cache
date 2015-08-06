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
terminate(_Reason, _State) ->
    ok.
code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
handle_info(info, State) ->
    {noreply, State}.
add_target_resource_type(Type) ->
    gen_server:cast(?SERVER, {add_target_resource_type, Type}).
add_local_resource(Type, Instance) ->
    gen_server:cast(?SERVER, {add_local_resource, {Type, Instance}}).

handle_call({fetch_resource, Type}, _, State) ->
    {reply, dict:find(Type, State#state.found_resource_tuples), State}.
handle_cast({add_target_resource_type, Type}, State) ->
    TargeTypes = State#state.target_resource_types,
    NewTargetTypes = [Type | lists:delete(Type, TargeTypes)],
    {noreply, State#state{target_resource_types = NewTargetTypes}};

handle_cast({add_local_resource, {Type, Instance}}, State) ->
    ResourceTuples = State#state.local_resource_tuples,
    NewResourceTuples = add_resource(Type, Instance, ResourceTuples),
    {noreply, State#state{local_resource_tuples = NewResourceTuples}};

handle_cast({trade_resource, {ReplyTo, Remotes}},
            #state{local_resource_tuples = Locals,
                   target_resource_types = TargetTypes,
                   found_resource_tuples = OldFound} = State) ->
    FilteredRemotes = resource_for_types(TargetTypes, Remotes),
    NewFound = add_resources(FilteredRemotes, OldFound),
    case ReplyTo of
        noreply ->
            ok;
        _  ->
            gen_server:cast({?SERVER, ReplyTo},
                            {trade_resource, {noreply, Locals}})
    end,
    {noreply, State#state{found_resource_tuples = NewFound}};
handle_cast(trade_resource, State) ->
    ResourceTuples = State#state.local_resource_tuples,
    AllNodes = [node() | nodes()],
    lists:foreach(
      fun(Node) ->
              gen_server:cast({?SERVER, Node}, {trade_resource, {node(), ResourceTuples}})
      end, AllNodes),
    {noreply, State}.

add_resource(Type, Resource, ResourceTuples) ->
    case  dict:find(Type, ResourceTuples) of
        {ok, _ResourceList} ->
            NewList = [Resource | lists:delete(Resource, ResourceTuples)],
            dict:store(Type, NewList, ResourceTuples);
        error ->
            dict:store(Type, Resource, ResourceTuples)
    end.

fetch_resource(Type) ->
    gen_server:call(?SERVER, {fetch_resource, Type}).

trade_resource() ->
    gen_server:cast(?SERVER, trade_resource).
add_resources([{Type, Resource}|T], ResourceTuples) ->
    add_resources(T, add_resource(Type, Resource, ResourceTuples));
add_resources([], ResourceTuples) ->
    ResourceTuples.

resource_for_types(Types, ResourceTuples) ->
    Fun =
        fun(Type, Acc) ->
                case dict:find(Type, ResourceTuples) of
                    {ok, List} ->
                        [{Type, Instance} || Instance <- List] ++ Acc;
                    error  ->
                        Acc
                end
        end,
    lists:fold1(Fun, [], Types).
