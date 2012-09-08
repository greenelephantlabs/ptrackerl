-type id() :: pos_integer()|string().

-record(token, {
		guid :: string(),
		id   :: integer()
		}).
-type token() :: #token{}.

-record(activity, {
		id      :: integer(),
		version :: integer(),
		event_type :: string(),
		occurred_at :: string(),
		author :: string(),
		project_id :: integer(),
		description :: string(),
		stories :: list()
		}).
-type activity() :: #token{}.

-record(project, {
		id          :: integer(),
		name        :: string(),
		point_scale :: list(),
		labels      :: list()
		}).
-type project() :: #project{}.

-record(person, {
		email    :: string(),
		name     :: string(),
		initials :: string()
		}).
-type person() :: #person{}.

-record(membership, {
		id                 :: integer(),
		person=#person{}   :: person(),
		role               :: string(),
		project=#project{} :: project()
		}).
-type membership() :: #membership{}.

-record(story, {
		id            :: integer(),
		project_id    :: integer(),
		story_type    :: string(),
		url           :: string(),
		estimate      :: integer(),
		current_state :: string(),
		description   :: string(),
		name          :: string(),
		requested_by  :: string(),
		owned_by      :: string(),
		created_at    :: string(),
		labels        :: list()
		}).
-type story() :: #story{}.

-record(iteration, {
          id :: integer(),
          number :: integer(),
          start :: string(),
          finish :: string(),
          team_strength :: number(),
          stories :: [story()]
         }).
-type iteration() :: #iteration{}.

-record(note, {
		text :: string()
		}).
-type note() :: #note{}.


