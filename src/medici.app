%% Medici application resource file.
{application, medici,
 [{description, "Medici Tokyo Tyrant interface"},
  {vsn, "0.5"},
  {modules, [medici,
	     medici_sup,
	     medici_controller,
	     medici_conn_sup,
	     medici_conn,
	     principe,
	     principe_table]},
  {registered, []},
  {applications, [kernel, stdlib]},
  {mod, {medici, []}},
  {env, []}
 ]}.
