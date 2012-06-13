-module(ptrackerl_pack).
-include("ptrackerl.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-export([story/2]).

-define(FIELD(Rec, Field, Fun),
	case Rec of
		undefined -> [];
		Value -> [{Field, [Fun(Value)]}]
	end
).

-define(FIELD(Rec, Field),
	?FIELD(Rec, Field, fun(X) -> X end)
).

%% PRIVATE FUNCTIONS
-spec pathx(string(), list()) -> term().
pathx(String, Xml) ->
	case xmerl_xpath:string(String, Xml) of
		[] -> nil;
		[Record] -> Record#xmlText.value
	end.

to_text(Tuple) ->
	Xml = xmerl:export_simple(Tuple, xmerl_xml),
	lists:flatten(Xml).

%% API
-spec story(atom(), record()) -> string().
story(pack, Record) ->
	Story = [{story,
				?FIELD(Record#story.story_type, story_type) ++
				?FIELD(Record#story.estimate, estimate) ++
				?FIELD(Record#story.current_state, current_state) ++
				?FIELD(Record#story.description, description) ++
				?FIELD(Record#story.name, name) ++
				?FIELD(Record#story.requested_by, requested_by) ++
				?FIELD(Record#story.owned_by, owned_by) ++
				?FIELD(Record#story.labels, labels, fun(X) -> string:join(X, ",") end)
				}],
	to_text(Story).
