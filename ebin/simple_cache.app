{application, simple_cache,
 [{description, "A simple caching system"},
  {vns, "0.1.0"},
  {modules, [sc_app, sc_sup]},
  {registered, [sc_sup]},
  {aplications, [kernel, stdlib]},
  {mod, {sc_app, []}}
 ]}
