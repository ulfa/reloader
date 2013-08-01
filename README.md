# Overview

This small tool reloads the modules in the ebin directory of an erlang 
application.

# Integration in your own app

To use it in your own app, you have to add the dependency in your rebar.config

```erlang
{reloader, ".*", {git, "git@github.com:ulfa/reloader.git", "master"}}
```
After starting reloader with -s reloader from the commandline or from your
application with ``` application:start(reloader) ``` the application
is able to receive the reload event.

From a remote node i use something like this : 

```erlang
	rpc:cast(Node, code_reloader, reload, [])
```

# Todo
