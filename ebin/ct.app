{application, 'ct', [
	{description, "New project"},
	{vsn, "0.1.0"},
	{modules, ['ct','ct_app','ct_sup']},
	{registered, [ct_sup]},
	{applications, [kernel,stdlib,cowboy]},
	{mod, {ct_app, []}},
	{env, []}
]}.