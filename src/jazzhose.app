{application, jazzhose,
 [{description, "jazzhose"},
  {vsn, "0.01"},
  {modules, [
    jazzhose,
    jazzhose_app,
    jazzhose_sup,
    jazzhose_web,
    jazzhose_deps
  ]},
  {registered, []},
  {mod, {jazzhose_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
