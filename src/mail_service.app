%% This is the application resource file (.app file) for the 'base'
%% application.
{application, mail_service,
[{description, "mail_service" },
{vsn, "0.0.1" },
{modules, [mail_service_app,mail_service_sup,
	   mail_service]},
{registered,[mail_service]},
{applications, [kernel,stdlib]},
{mod, {mail_service_app,[]}},
{start_phases, []}
]}.
