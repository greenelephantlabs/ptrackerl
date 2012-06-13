-record(token, {
		guid :: string(),
		id   :: integer()
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


