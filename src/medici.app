%% Medici application resource file.
{application, medici,
 [{description, "Medici Tokyo Tyrant interface"},
  {vsn, "0.5"},
  {modules, [medici_app,
	     medici_sup,
	     medici_controller,
	     medici_client_sup,
	     medici_client,
	     principe,
	     principe_table]},
  {registered, []},
  {applications, [kernel, stdlib]},
  {mod, {medici_app, []}},
  {env, []}
 ]}.
