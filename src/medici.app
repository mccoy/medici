%% Medici application resource file.
%%
%% Add runtime options in the list value for the options property of
%% env or set them via the command-line or via a config file.
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
  {env, [{options, []}]}
 ]}.
