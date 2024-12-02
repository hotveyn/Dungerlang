{application, dungeon, [
    {description, "An OTP application"},
    {vsn, "0.1.0"},
    {modules, [ dungeon_app, dungeon_sup, dungeon_server ]},
    {registered, [dungeon_sup]},
    {mod, {dungeon_app, []}},
    {applications, [
        kernel,
        stdlib
    ]},
]}.
