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

% STORIES
ptrackerl:stories("PROJECT_ID", all).
ptrackerl:stories("PROJECT_ID", {find, "STORY_ID"}).
R = #story{description = "My Description", story_type = "feature", name = "Story Name"}.
ptrackerl:stories("PROJECT_ID", {add, R}).