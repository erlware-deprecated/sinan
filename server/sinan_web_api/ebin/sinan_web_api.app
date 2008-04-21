%% -*- mode: Erlang; fill-column: 75; comment-column: 50; -*-

{application, sinan_web_api,
 [{description, "Build system for erlang"},
  {vsn, "0.1.0.0"},
  {modules, [sinan_web_api,
             swa_app,
             swa_sup,
             swa_dumb_server,
             swa_event_handler,
             swa_crary_handler]},
  {registered, [sin_sup]},
  {versioned_dependencies, [{crary, "0.1.1", gte},
                            {sinan, "0.10.0.0", gte}]},
  {applications, [kernel, stdlib, sinan, crary, uri]},
  {mod, {swa_app, []}}]}.
