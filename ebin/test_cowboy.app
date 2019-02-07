{application, 'test_cowboy', [
	{description, "New project"},
	{vsn, "0.1.0"},
	{modules, ['req_api','rest_h','rest_h1','test_cowboy','test_cowboy_app','test_cowboy_sup','ws_h']},
	{registered, [test_cowboy_sup]},
	{applications, [kernel,stdlib,cowboy,jiffy]},
	{mod, {test_cowboy_app, []}},
	{env, []}
]}.