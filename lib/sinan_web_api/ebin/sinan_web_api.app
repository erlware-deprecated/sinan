%% -*- mode: Erlang; fill-column: 75; comment-column: 50; -*-

{application, sinan,
 [{description, "Build system for erlang"},
  {vsn, "0.1.0.0"},
  {modules, [sinan_web_api, swa_app, swa_sup]},
  {registered, [sin_sup]},
  {versioned_dependencies, [{crary, "0.1.1", gte}]},
  {applications, [kernel, stdlib, sinan, crary]},
  {mod, {swa_app, []}}]}.
