-module(ptrackerl).
-author("Gustavo Chain <gustavo@inaka.net>").
-vsn("0.2").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/0, get/1, set/2]).
-export([
         token/2,
         activities/0, activities/1, activities/2,
         story_activities/1, story_milestones/1,
         projects/0, projects/1, projects/2,
         memberships/1, memberships/2, memberships/3,
         iterations/1, iterations/2, iterations/3,
         stories/1, stories/2, stories/3, stories/4,
         notes/4,
         tasks/2, tasks/3, tasks/4,

         report/0, report/1
        ]
       ).

-include("ptrackerl.hrl").
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SPECS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start() -> pid().
-spec get(atom()) -> term().
-spec set(atom(), term()) -> term().
-spec token(string(),string()) -> term().
-spec activities() -> term().
-spec activities(id()) -> term().
-spec activities(id(), tuple()) -> term().
-spec projects() -> term().
-spec projects(id()) -> term().
-spec projects(add, project()) -> term().
-spec memberships(id()) -> term().
-spec memberships(id(),id()) -> term().
-spec memberships(id(), add, membership()) -> term().
-spec iterations(id()) -> term().
-spec iterations(id(), tuple()) -> term().
-spec iterations(id(),atom(),tuple()) -> term().
-spec stories(id()) -> term().
-spec stories(id(),id()) -> term().
-spec stories(id(), create|delete, story()|id()) -> term().
-spec stories(id(), update, id(), story()) -> term().
-spec notes(id(), id(), create, note()) -> term().
-spec tasks(id(),id()) -> term().
-spec tasks(id(),id(),id()) -> term().
-spec tasks(id(),id(),delete,id()) -> term().
-spec init([]) -> {ok, term()}.

-spec handle_call(tuple(), pid(), term()) -> term().
-spec handle_cast(tuple(), term())        -> term().
-spec handle_info(term(), term())         -> term().
-spec terminate(term(), term())           -> term().
-spec code_change(term(), term(), term()) -> term().

-record(state, {
          token :: string()
         }).

-record(action, {
          method    = get :: get|post|put|delete,
          content_type    :: undefined|xml
         }).

-record(item, {
          path      = ["projects"]                  :: list(),
          use_token = true                          :: boolean(),
          actions = [
                     { get,    #action{} },
                     { create, #action{method = post,   content_type = xml} },
                     { update, #action{method = update, content_type = xml} },
                     { delete, #action{method = delete} }] :: list()
         }).

-define(MIME_TYPES, [{xml, "application/xml"}]).
-define(API_METHODS, [
                      {token, #item{
                         path = ["tokens"],
                         use_token = false,
                         actions = [{ retrieve, #action{method = post} }]
                        }
                      },
                      {activities,  #item{path = ["activities"]} },
                      {projects,    #item{} },
                      {memberships, #item{} },
                      {iterations,  #item{actions = [{ get, #action{} }] }},
                      {stories,     #item{} },
                      {story_activities, #item{path = ["stories"]} },
                      {notes,       #item{} },
                      {tasks,       #item{} }
                     ]
       ).
-define(API_URL, "https://www.pivotaltracker.com/services/v4").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start() -> gen_server:start({local, ?MODULE}, ?MODULE, [], []).
get(token) -> gen_server:call(?MODULE, {state, get, token}).
set(token, Value) -> gen_server:call(?MODULE, {state, set, token, Value}).

%% Token
token(Username, Password) -> api(token, retrieve, ["active"], [{username, Username}, {password, Password}]).

%% Activities
activities()            -> api(activities, get, "").
activities(Id)          -> api(projects, get, [Id, "activities"]).
activities(Id, Filters) -> api(projects, get, [Id, "activities" ++ "?" ++ build_params(Filters)]).

story_activities(Id)    -> api(story_activities, get, [Id, "activities"]).
story_milestones(Id)    ->
    {200, Acts} = story_activities(Id),
    [ {CS, OA, Author} || A = #activity{occurred_at = OA,
                                        author = Author,
                                        stories = [#story{current_state = CS} = S]} <- Acts, CS /= nil ].

projects()            -> api(projects, get, "").
projects(Id)          -> api(projects, get, [Id]).
projects(add, Record) -> api(projects, create, "", [ptrackerl_pack:project(pack, Record)]);
projects(remove, Id)  -> api(projects, delete, [Id]).

memberships(ProjectId)              -> api(memberships, get,    [ProjectId, "memberships"]).
memberships(ProjectId, Id)          -> api(memberships, get,    [ProjectId, "memberships", Id]).
memberships(ProjectId, add, Record) -> api(memberships, create, [ProjectId, "memberships"], [ptrackerl_pack:membership(pack, Record)]);
memberships(ProjectId, remove, Id)  -> api(memberships, delete, [ProjectId, "memberships", Id]).

iterations(ProjectId)            -> api(iterations, get, [ProjectId, "iterations"]).
iterations(ProjectId,
           [{_,_}|_] = Filters) -> api(iterations, get, [ProjectId, "iterations?" ++ build_params(Filters)]);
iterations(ProjectId, GroupedBy) -> api(iterations, get, [ProjectId, "iterations", GroupedBy]).
iterations(ProjectId,
           GroupedBy,
           [{_,_}|_] = Filters) -> api(iterations, get, [ProjectId, "iterations", to_string(GroupedBy) ++ "?" ++ build_params(Filters)]).

stories(ProjectId)                     -> api(stories, get, [ProjectId, "stories"]).
stories(ProjectId, Id)                 -> api(stories, get, [ProjectId, "stories", Id]).
stories(ProjectId, create, Record)     -> api(stories, add, [ProjectId, "stories"], ptrackerl_pack:story(pack, Record));
stories(ProjectId, delete, Id)         -> api(stories, del, [ProjectId, "stories", Id]).
stories(ProjectId, update, Id, Record) -> api(stories, put, [ProjectId, "stories", Id], ptrackerl_pack:story(pack, Record)).

notes(ProjectId, StoryId, create, Record) -> api(notes, get, [ProjectId, "stories", StoryId, "notes"], ptrackerl_pack:note(pack, Record)).

tasks(ProjectId, StoryId)                 -> api(tasks, get, [ProjectId, "stories", StoryId, "tasks"]).
tasks(ProjectId, StoryId, TaskId)         -> api(tasks, get, [ProjectId, "stories", StoryId, "tasks", TaskId]).
tasks(ProjectId, StoryId, delete, TaskId) -> api(tasks, delete, [ProjectId, "stories", StoryId, "tasks", TaskId]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% GEN SERVER FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([]) -> { ok, #state{} }.
handle_call({ state, get, Key }, _From, State) ->
    Value = case Key of
                token -> State#state.token;
                _ -> undefined
            end,
    {reply, Value, State};
handle_call({ state, set, Key, Value }, _From, State) ->
    case Key of
        token -> {reply, ok, State#state{token = to_string(Value)}};
        _ -> {reply, not_found, State}
    end;
handle_call({api, {ItemName, ActionName, Args, Body}}, _From, State) ->
    {Item, Action} = record_for(ItemName, ActionName),
                                                % Url
    Url = build_url(Item#item.path ++ Args),

                                                % Headers
    Headers = case Action#action.content_type of
                  undefined -> [];
                  Type -> [{"Content-Type",proplists:get_value(Type, ?MIME_TYPES)}]
              end ++
        case Item#item.use_token of
            true -> [{"X-TrackerToken", State#state.token}];
            _ -> []
        end,

    {ok, Status, _Headers, Response} = request(Url, Action#action.method, Headers, Body),
    case Status of
        "200" -> {reply, {200, ptrackerl_parser:pack({ItemName, Response})}, State};
        "500" -> {reply, {500, "error"}, State};
        _     -> {reply, {Status, Response}, State}
    end.

handle_cast(_, State) -> { ok, State }.
handle_info(Msg, State) -> { Msg, State }.
terminate(Reason, State) -> { Reason, State }.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PRIVATE FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
request(Url, Method, Headers, Params) ->
    Key = {Url, Method, Headers, Params},
    case erlang:get(Key) of
        undefined ->
            Body = build_params(Params),
            Res = ibrowse:send_req(Url, Headers, Method, Body),
            erlang:put(Key, Res),
            Res;
        Res ->
            Res
    end.

api(ItemName, ActionName, Args) -> api(ItemName, ActionName, Args, "").
api(ItemName, ActionName, Args, Body) ->
    gen_server:call(?MODULE, {api, {ItemName, ActionName, Args, Body}}, 300000).

record_for(ItemName, ActionName) ->
    Item = proplists:get_value(ItemName, ?API_METHODS),
    Action = proplists:get_value(ActionName, Item#item.actions),
    { Item, Action }.

build_url(Args) ->
    Base = [?API_URL],
    Args2 = lists:map(fun(X) -> to_string(X) end, Args),
    string:join(Base ++ Args2, "/").

build_params(Params) ->
    List = lists:map(fun(X) -> format_param(X) end, Params),
    string:join(List, "&").

format_param({Key,Value}) ->
    string:join([to_string(Key), to_string(Value)], "=");
format_param(String) -> to_string(String).

to_string(Value) when is_atom(Value) -> atom_to_list(Value);
to_string(Value) when is_integer(Value) -> integer_to_list(Value);
to_string(Value) -> Value.



report() ->
    report(["519173", "61492"]).

report(ProjectIds) ->
    report(ProjectIds, "2012/11/01 00:00:01 UTC", "2012/11/31 23:59:59 UTC").

report(ProjectIds, Start, Stop) ->
    aggregate([ {PId, report_project(PId, Start, Stop)} || PId <- ProjectIds ]).

report_project(PId, Start, Stop) ->
    io:format("Getting stories for ~p~n", [PId]),
    {200, Stories} = ptrackerl:stories(PId),
    io:format("Done getting stories for ~p~n", [PId]),
    SIds = [ {Id, Est, St} || S = #story{id = Id, estimate = Est, current_state = St} <- Stories, 
                              (St == "accepted") ],
    io:format("Getting stories' milestones' for ~b stories~n", [length(SIds)]),
    Milestones = [ {SId, Est, State, story_milestones(SId)} || {SId, Est, State} <- SIds ],
    io:format("Done getting stories' milestones~n", []),
    Milestones2 = 
        [ X || {_, _, _, M} = X <- Milestones, between(X, Start, Stop) == true ],
    [ {SId, delivery_author(X), list_to_integer(Est), accept_day(X) } || {SId, Est, _, _} = X <- Milestones2, Est /= nil ].

aggregate(X) ->
    io:format("Debug all: ~n~p~n", [X]),
    {_, X2} = lists:unzip(X),
    X3 = lists:flatten(X2),
    PerDay0 = [ {D ++ " " ++ A, E} || {_, A, E, D} <- X3 ],
    PerDay = lists:foldl(fun({K, V}, A) ->
                                 orddict:append(K, V, A)
                         end, orddict:new(), PerDay0),
    PerAuthor = lists:foldl(fun({_, A, E, _}, Acc) ->
                                    orddict:append(A, E, Acc)
                            end, orddict:new(), X3),
    io:format("~n~nBy day:~n"),
    [ io:format("~s: ~b = ~p~n", [K, lists:sum([ X || X <- V, X > 0 ]), V]) || {K, V} <- PerDay ],
    io:format("~nBy author:~n"),
    [ io:format("~s: ~b = ~p~n", [K, lists:sum([ X || X <- V, X > 0 ]), V]) || {K, V} <- PerAuthor ],
    io:format("~n~n"),
    ok.

accept_day(X) ->
    case accept_date(X) of
        undefined -> undefined;
        Date ->
            hd(string:tokens(Date, " "))
    end.
    
accept_date({_, _, _, M}) ->
    case lists:keyfind("accepted", 1, M) of
        {_, O, _} ->
            O;
        _ ->
            undefined
    end.
           
delivery_author({_, _, _, M}) ->
    case lists:keyfind("delivered", 1, M) of
        false ->
            case lists:keyfind("started", 1, M) of
                false ->
                    case lists:keyfind("accepted", 1, M) of
                        false ->
                            undefined;
                        {_, _, A} ->
                            A
                    end;
                {_, _, A} ->
                    A
            end;
        {_,_,A} ->
            A
    end.

between(M, Start, Stop) ->
    case accept_date(M) of
        undefined -> false;
        X ->
            between0(X, Start, Stop)
    end.
 
between0(O, Start, Stop) ->
    (Start =< O) and (O =< Stop).
        
