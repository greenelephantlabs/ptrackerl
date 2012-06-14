-record(token, {
		guid :: string(),
		id   :: integer()
		}).

-record(project, {
		id          :: integer(),
		name        :: string(),
		point_scale :: list(),
		labels      :: list()
		}).

-record(person, {
		email    :: string(),
		name     :: string(),
		initials :: string()
		}).

-record(membership, {
		id                 :: integer(),
		person=#person{},
		role               :: string(),
		project=#project{}
		}).

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


