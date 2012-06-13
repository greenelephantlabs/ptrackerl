-module(ptrackerl).
-author("Gustavo Chain <gustavo@inaka.net>").
-vsn("0.1").

-behaviour(gen_server).

-type start_result() :: {ok, pid()} | {error, {already_started, pid()}} | {error, term()}.

-include("ptrackerl.hrl").

%% API
-export([start/0, update/2,
	token/2, activity/0, activity/1, activity/2, projects/1, stories/2, tasks/3, api/1, api/2]).
%% GEN SERVER
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([test/0]).

-record(state, {
		token :: string()
		}).
-opaque state() :: #state{}.

-record(request, {
		url              :: string(),
		method = get     :: atom(),
		headers = []     :: tuple(),
		params = []      :: string()
		}).
-opaque request() :: #request{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec start() -> start_result().
start() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec update(atom(), term()) -> Response::term().
update(token, Token) ->
	gen_server:call(?MODULE, {update, token, Token}).

%% Token
-spec token(list(), list()) -> Response::term().
token(Username, Password) ->
	gen_server:call(?MODULE, {token, Username, Password}).

%% Activity
-spec activity() -> Response::term().
activity() ->
	activity(undefined, []).

-spec activity(list()|string()) -> Response::term().
activity([{_,_}|_] = Filters) ->
	activity(undefined, Filters);
activity(ProjectId) ->
	activity(ProjectId, []).

-spec activity(string(),list()) -> Response::term().
activity(ProjectId, Filters) ->
	gen_server:call(?MODULE, {activity, {project_id, ProjectId, filters, Filters}}).

%% Projects
-spec projects(atom()|tuple()) -> Response::term().
projects(all) ->
	gen_server:call(?MODULE, {projects, all});
projects({find, ProjectId}) ->
	gen_server:call(?MODULE, {projects, {find, ProjectId}}).

%% Stories
-spec stories(list(), atom()|tuple()) -> Response::term().
stories(ProjectId, all) ->
	gen_server:call(?MODULE, {stories, {ProjectId, all}});
stories(ProjectId, {find, StoryId}) ->
	gen_server:call(?MODULE, {stories, {ProjectId, {find, StoryId}}});
stories(ProjectId, {add, StoryRecord}) ->
	gen_server:call(?MODULE, {stories, {ProjectId, {add, StoryRecord}}}).

%% Tasks
-spec tasks(list(), list(), atom()|tuple()) -> Response::term().
tasks(ProjectId, StoryId, all) ->
	gen_server:call(?MODULE, {tasks, {ProjectId, StoryId, all}});
tasks(ProjectId, StoryId, {find, TaskId}) ->
	gen_server:call(?MODULE, {tasks, {ProjectId, StoryId, {find, TaskId}}}).

-spec api(request()) -> tuple().
api(Request) ->
	api(Request, undefined).

-spec api(request(), string()) -> tuple().
api(Request, Token) ->
	Url = build_url(Request#request.url),
	Headers = Request#request.headers ++ case Token of
		undefined -> [];
		_ -> [{"X-TrackerToken",Token}]
	end,
	Method = Request#request.method,
	Params = build_params(Request#request.params),
	
	io:format("URL:     ~p\n", [Url]),
	io:format("Headers: ~p\n", [Headers]),
	io:format("Method:  ~p\n", [Method]),
	io:format("Params:  ~p\n", [Params]),
	
	{ok, Status, _Headers, Body} = ibrowse:send_req(Url, Headers, Method, Params),
	{status(Status), Body}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% GEN SERVER FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec init(list()) -> {ok, state()}.
init([]) ->
	{ok, #state{token = ""}}.

-spec handle_call(tuple(),reference(), state()) -> {reply, ok, state()}.
handle_call({update, token, Token}, _From, State) ->
	{ reply, ok, State#state{token=Token} };

handle_call({token, Username, Password}, _From, State) ->
	Request = #request{
			url = ["tokens", "active"],
			method = post,
			params = [{username, Username}, {password, Password}]
			},
	{reply, api(Request), State};

handle_call({activity}, _From, State) ->
	Token = State#state.token,
	Request = #request{ url = ["activities"] },
	{reply, api(Request, Token), State};

handle_call({activity, {project_id, ProjectId, filters, Filters}}, _From, State) ->
	Token = State#state.token,
	Params = build_params(Filters),
	Url = case ProjectId of
		undefined -> [];
		_ -> ["projects", ProjectId]
	end ++ ["activities?" ++ Params],
	Request = #request{ url = Url },
	{reply, api(Request, Token), State};

handle_call({projects, Action}, _From, State) ->
	Token = State#state.token,
	Url = case Action of
		all -> ["projects"];
		{find, Id} -> ["projects", Id]
	end,
	Request = #request{ url = Url },
	{reply, api(Request, Token), State};

handle_call({stories, {ProjectId, Action}}, _From, State) ->
	Token = State#state.token,
	Request = case Action of
		all ->
			#request{
				url = ["projects", ProjectId, "stories"]
			};
		{find, Id} ->
			#request{
				url = ["projects", ProjectId, "stories", Id]
			};
		{add, StoryRecord} ->
			#request{
				url = ["projects", ProjectId, "stories"],
				method = post,
				headers = [{"Content-Type", "application/xml"}],
				params = [ptrackerl_pack:story(pack, StoryRecord)]
			}
	end,
	{reply, api(Request, Token), State};

handle_call({tasks, {ProjectId, StoryId, Action}}, _From, State) ->
	Token = State#state.token,
	Url = case Action of
		all -> ["projects", ProjectId, "stories", StoryId, "tasks"];
		{find, Id} -> ["projects", ProjectId, "stories", StoryId, "tasks", Id]
	end,
	Request = #request{ url = Url },
	{reply, api(Request, Token), State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_P, State) ->
	{noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(_Info, State) ->
	{noreply, State}.

-spec terminate(any(), state()) -> any().
terminate(_Reason, _State) ->
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PRIVATE FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
build_url(Args) ->
	Base = ["https://www.pivotaltracker.com/services/v3"],
%	Base = ["http://localhost:10000/services/v3"],
	string:join(Base ++ Args, "/").

build_params(Params) ->
	List = lists:map(fun(X) -> format_param(X) end, Params),
	string:join(List, "&").

format_param({Key,Value}) ->
	string:join([atom_to_list(Key), Value], "=");
format_param(String) -> String.

-spec status(string()) -> integer()|atom().
status("200") -> 200;
status("400") -> 400;
status("401") -> 401;
status(_)     -> undefined.

test() ->
	ok.
