-module(ptrackerl).
-author("Gustavo Chain <gustavo@inaka.net>").
-vsn("0.1").

-behaviour(gen_server).

-type start_result() :: {ok, pid()} | {error, {already_started, pid()}} | {error, term()}.

%% API
-export([start/0, update/2,
	token/2, projects/0, projects/1, api/3, api/4]).
%% GEN SERVER
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
		token ::string()
		}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec start() -> start_result().
start() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

update(token, Token) ->
	gen_server:call(?MODULE, {update, token, Token}).

token(Username, Password) ->
	gen_server:call(?MODULE, {token, Username, Password}).

projects() ->
	gen_server:call(?MODULE, {projects, all}).
projects(ProjectId) ->
	gen_server:call(?MODULE, {projects, ProjectId}).

api(Url, Method, Param) ->
	api(Url, Method, Param, none).
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
init([]) ->
	{ok, #state{token = ""}}.

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

handle_call({projects, Id}, _From, State) ->
	Token = State#state.token,
	Url = case Id of
		all -> build_url(["projects"]);
		_ ->   build_url(["projects", Id])
	end,
	Api = api(Url, get, [], Token),
	io:format("Api: ~p\n", [Api]),
	{reply, ok, State}.

handle_cast(_P, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Foo, _Bar) ->
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
