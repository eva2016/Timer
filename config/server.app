{
    application, server,
    [
        {description, "The server."},
        {vsn, "0.1.0"},
        {modules, [server]},
        {registered, [server_sup]},
        {applications, [kernel, stdlib, sasl]},
        {mod, {server, []}},
        {start_phases, []}
    ]    
}.
