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
ptrackerl:set(token, T). % Set token for further requests
%
% PROJECTS
ptrackerl:projects(). % retrieve all projects
ptrackerl:projects("PROJECT_ID"). % retrieve project with id "PROJECT_ID"
Project = #project{name = "Project Name", point_scale = ["1","2","3"]}.
ptrackerl:projects({add, Project}).
ptrackerl:projects(remove, "PROJECT_ID").
%
% STORIES
ptrackerl:stories("PROJECT_ID", all).
ptrackerl:stories("PROJECT_ID", {find, "STORY_ID"}).
Story = #story{description = "My Description", story_type = "feature", name = "Story Name"}.
ptrackerl:stories("PROJECT_ID", {add, Story}).
```

Reporting
---------

1. Get all projects
2. Get all stories for each project for last months
3. Get activity for each story:
   1. Check if story is accepted
   2. If accepted check who and when has delivered it
