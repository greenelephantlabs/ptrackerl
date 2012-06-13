-module(ptrackerl).
-author("Gustavo Chain <gustavo@inaka.net>").
-vsn("0.1").

-behaviour(gen_server).

-type start_result() :: {ok, pid()} | {error, {already_started, pid()}} | {error, term()}.

%% API
-export([start/0, update/2,
	token/2, projects/1, stories/2, tasks/3, api/3, api/4]).
%% GEN SERVER
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
		token ::string()
		}).
-opaque state() :: #state{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec start() -> start_result().
start() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec update(atom(), term()) -> Response::term().
update(token, Token) ->
	gen_server:call(?MODULE, {update, token, Token}).

-spec token(list(), list()) -> Response::term().
token(Username, Password) ->
	gen_server:call(?MODULE, {token, Username, Password}).

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
stories(ProjectId, {add, StoryDetail}) ->
	gen_server:call(?MODULE, {stories, {ProjectId, {add, StoryDetail}}}).

%% Tasks
-spec tasks(list(), list(), atom()|tuple()) -> Response::term().
tasks(ProjectId, StoryId, all) ->
	gen_server:call(?MODULE, {tasks, {ProjectId, StoryId, all}});
tasks(ProjectId, StoryId, {find, TaskId}) ->
	gen_server:call(?MODULE, {tasks, {ProjectId, StoryId, {find, TaskId}}}).

-spec api(list(),atom(),tuple()) -> tuple().
api(Url, Method, Param) ->
	api(Url, Method, Param, none).

-spec api(list(),atom(),tuple(),list()) -> tuple().
api(Url, Method, Params, Token) ->
	Formatted = build_params(Params),
	Header = case Token of
		none ->
			[];
		_ ->
			[{"X-TrackerToken", Token}]
	end,
	{ok,Status,_Headers,Body} = ibrowse:send_req(Url, Header, Method, Formatted),
	case Status of
		"200" ->
			{Status, erlsom:simple_form(Body)};
		_ ->
			{Status, Body}
	end.


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
	Url = build_url(["tokens", "active"]),
	case api(Url, post, [{username,Username}, {password,Password}]) of
		{"200", XML} ->
			{ok,{"token",[],[{"guid",[],[Token]},_]},_} = XML,
			{reply, {ok, Token}, State};
		{_, Error} ->
			{reply, {error, Error}, State}
	end;

handle_call({projects, Action}, _From, State) ->
	Token = State#state.token,
	Url = case Action of
		all -> build_url(["projects"]);
		{find, Id} -> build_url(["projects", Id])
	end,
	Api = api(Url, get, [], Token),
	io:format("Api: ~p\n", [Api]),
	{reply, ok, State};

handle_call({stories, {ProjectId, Action}}, _From, State) ->
	Token = State#state.token,
	{Url, Method} = case Action of
		all -> { build_url(["projects", ProjectId, "stories"]), get };
		{find, Id} -> { build_url(["projects", ProjectId, "stories", Id]), get };
		{add, StoryDetail} -> { build_url(["projects", ProjectId, "stories"]), post }
	end,
	Api = api(Url, Method, [], Token),
	{reply, Api, State};

handle_call({tasks, {ProjectId, StoryId, Action}}, _From, State) ->
	Token = State#state.token,
	Url = case Action of
		all -> build_url(["projects", ProjectId, "stories", StoryId, "tasks"]);
		{find, Id} -> build_url(["projects", ProjectId, "stories", StoryId, "tasks", Id])
	end,
	Api = api(Url, get, [], Token),
	{reply, Api, State}.

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
%	Base = ["http://localhost:10000"],
	string:join(Base ++ Args, "/").

build_params(Params) ->
	List = lists:map(fun(X) -> format_param(X) end, Params),
	string:join(List, "&").

format_param({Key,Value}) ->
	string:join([atom_to_list(Key), Value], "=");
format_param(String) -> String.
