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
do_pack(projects, Xml) ->
    lists:map(fun(Project) ->
                      #project{id = pathx("/project/id/text()", Project),
                               name = pathx("/project/name/text()", Project)
                              }
              end, xmerl_xpath:string("//projects/project", Xml));

do_pack(token, Xml) ->
	#token{
		guid = pathx("//token/guid/text()", Xml),
		id   = pathx("//token/id/text()", Xml)
		};

do_pack(story_activities, Xml) ->
    lists:keysort(
      #activity.occurred_at,
      lists:map(fun(Act) ->
                        do_pack(activities, Act)
                end, xmerl_xpath:string("//activities/activity", Xml))
     );

do_pack(activities, Xml) ->
    #activity{
         id = pathx("//activity/id/text()", Xml),
         description = pathx("//activity/description/text()", Xml),
         occurred_at = pathx("//activity/occurred_at/text()", Xml),
         event_type = pathx("//activity/event_type/text()", Xml),
         author = pathx("//activity/author/person/initials/text()", Xml),
         stories = do_pack(stories, Xml)
        };

do_pack(iterations, Xml) ->
    lists:map(fun(Act) ->
                      #iteration{id = pathx("//iteration/id/text()", Act),
                                 number = pathx("//iteration/number/text()", Act),
                                 start = pathx("//iteration/start/text()", Act),
                                 finish = pathx("//iteration/finish/text()", Act),
                                 stories = do_pack(stories, Act)}
              end, xmerl_xpath:string("//iterations/iteration", Xml));

do_pack(stories, Xml) ->
    lists:map(fun(Act) ->
                      do_pack(story, Act)
              end, xmerl_xpath:string("//stories/story", Xml));

do_pack(story, Xml) ->
    #story{id   = pathx("//story/id/text()", Xml),
           name = pathx("//story/current_name/text() | //story/name/text()", Xml),
           estimate = pathx("//story/current_estimate/text() | //story/estimate/text()", Xml),
           description = pathx("//story/description/text()", Xml),
           current_state = pathx("//story/current_state/text()", Xml)
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
