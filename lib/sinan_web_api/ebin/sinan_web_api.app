%% -*- mode: Erlang; fill-column: 75; comment-column: 50; -*-

{application, sinan_web_api,
 [{description, "Build system for erlang"},
  {vsn, "0.1.0.6"},
  {modules, [sinan_web_api,
             swa_output_handler,
             swa_app,
             swa_sup,
             swa_event_handler,
             swa_crary_handler]},
  {registered, [swa_sup]},
  {versioned_dependencies, [{gtime, "0.9.4", gte},
                            {crary, "0.2.3", gte},
                            {sinan, "0.10.0.2", gte}]},
  {applications, [kernel, stdlib, sinan, crary, uri]},
  {mod, {swa_app, []}}]}.
