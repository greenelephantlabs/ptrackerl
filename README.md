PTrackERL
=========
Pivotal Tracker API Client written in Erlang

Compiling and Running
---------------------

```bash
make && make shell
```

Example
-------

```erlang
rr(ptrackerl).
ptrackerl:start().
ptrackerl:token("yourusername@host.com", "yourpassword").
T = "YOUR TOKEN". % See token response
ptrackerl:update(token, T). % Set token for further requests

% PROJECTS
ptrackerl:projects(all).
ptrackerl:projects({find, "PROJECT_ID").
Project = #project{name = "Project Name", point_scale = ["1","2","3"]}.
ptrackerl:projects({add, Project}).

% STORIES
ptrackerl:stories("PROJECT_ID", all).
ptrackerl:stories("PROJECT_ID", {find, "STORY_ID"}).
Story = #story{description = "My Description", story_type = "feature", name = "Story Name"}.
ptrackerl:stories("PROJECT_ID", {add, Story}).