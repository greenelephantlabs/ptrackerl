-module(ptrackerl_parser).
-include("ptrackerl.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-export([pack/1, unpack/1]).

-define(FIELD(Rec, Field, Fun),
	case Rec of
		undefined -> [];
		Value -> [{Field, [Fun(Value)]}]
	end
).

-define(FIELD(Rec, Field),
	?FIELD(Rec, Field, fun(X) -> X end)
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SPECS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec pack({token|atom(),string()}) -> token().
-spec unpack({token|project|atom(),term()}) -> string().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pack({Item, XmlString}) ->
	{Xml, _Rest} = xmerl_scan:string(XmlString, [{space, normalize}]),
	do_pack(Item, Xml).

unpack({Item, Record}) ->
	Xml = do_unpack(Item, Record),
	to_text(Xml).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PRIVATE FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do_pack(token, Xml) ->
	#token{
		guid = pathx("//token/guid/text()", Xml),
		id   = pathx("//token/id/text()", Xml)
		};

do_pack(activities, Xml) ->
	#activity{
		id = pathx("//activity/id/text()", Xml)
		};

do_pack(memberships, Xml) ->
  lists:map(fun(Membership) ->
                 #membership{
                             id = list_to_integer(pathx("//membership/id/text()",Membership)),
                             role = pathx("//membership/role/text()",Membership),
                             person=#person{
                                            email = pathx("//membership/person/email/text()",Membership),
                                            name = pathx("//membership/person/name/text()",Membership),
                                            initials = pathx("//membership/person/initials/text()",Membership)
                                           },
                             project=#project{
                                              id = list_to_integer(pathx("//membership/project/id/text()",Membership)) ,
                                              name = pathx("//membership/project/name/text()",Membership)
                                              }
                            }
            end,xmerl_xpath:string("//memberships/membership",Xml));

do_pack(_Item, Xml) ->
	Xml.

do_unpack(token, Record) ->
	{token,
		?FIELD(Record#token.id, id) ++
		?FIELD(Record#token.guid, guid)
		};

do_unpack(project, Record) ->
	{project,
		?FIELD(Record#project.name, name) ++
		?FIELD(Record#project.point_scale, point_scale, fun(X) -> string:join(X, ",") end) ++
		?FIELD(Record#project.labels, labels, fun(X) -> string:join(X, ",") end)
		}.

%% HELPERS
pathx(String, Xml) ->
	case xmerl_xpath:string(String, Xml) of
		[] -> nil;
		Records -> lists:foldl(fun(R,String) -> String ++ R#xmlText.value end,"", Records)  %% unicode characters cause multiple records to be returned;must join them together
	end.

to_text(Tuple) ->
	io:format("~p\n", [Tuple]),
	Xml = xmerl:export_simple([Tuple], xmerl_xml),
	lists:flatten(Xml).
