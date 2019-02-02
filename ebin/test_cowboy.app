{application, 'test_cowboy', [
	{description, "New project"},
	{vsn, "0.1.0"},
	{modules, ['test_cowboy','test_cowboy_app','test_cowboy_sup']},
	{registered, [test_cowboy_sup]},
	{applications, [kernel,stdlib,cowboy]},
	{mod, {test_cowboy_app, []}},
	{env, []}
]}.