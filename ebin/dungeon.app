%% -*- mode: Erlang; fill-column: 75; comment-column: 50; -*-

{application, dungeon,
 [{description, "RPC server for Erlang and OTP in action"},
  {vsn, "0.1.0"},
  {modules, [dungeon_app,
             dungeon_sup,
             dungeon_server]},
  {registered, [dungeon_sup, dungeon_server]},
  {applications, [kernel, stdlib]},
  {mod, {dungeon_app, []}}
 ]}.