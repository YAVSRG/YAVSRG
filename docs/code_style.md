# YAVSRG code style guidelines

### Commit messages

Use [semantic commit messages](https://gist.github.com/joshbuchea/6f47e86d2510bce28f8e7f42ae84c716)  
e.g. `feat: added a new button in ...`, `fix: crash when ...`

### Code style
When in doubt follow the [F# coding conventions](https://learn.microsoft.com/en-us/dotnet/fsharp/style-guide/conventions)

Use descriptive variable names  
One-letter variable names are not descriptive unless they follow specific conventions:  
- `i` = generic loop counter variable
- `k` = codebase convention for 'key' as the rhythm game concept, used when looping through keys/columns

Prefer `snake_case` variable names over `camelCase`

Use type annotations on all properties, arguments and return types for new code  
Bad: `let calculate(rate, chart) = ...`
Good `let calculate(rate: Rate, chart: Chart) : Difficulty = ...`

Do not use currying in methods top-level module functions for new code  
Bad: `let calculate (rate: Rate) (chart: Chart) : Difficulty = ...`
Good: `let calculate(rate: Rate, chart: Chart) : Difficulty = ...`

Use exceptions for control flow sparingly, they should be for exceptional cases and not hot paths

Do not use Option<'T> for error handling, use Result<'T, ...> instead  
The caller has more context on what to log rather than logging at the call site and then returning None
```fsharp
// Bad
let beatmap_from_file (path: string) : Beatmap option =
	try
		...
		Some beatmap
	with err ->
		Logging.Error(... err ...)
		None
		
let caller () =
	match beatmap_from_file "my_file.osu" with
	| Some beatmap -> ...
	| None -> Logging.Error("Log twice and without context")
		
// OK
let beatmap_from_file (path: string) : Result<Beatmap, exn> =
	try
		...
		Ok beatmap
	with
	| :? RelevantException err -> Error err
		
let caller () =
	match beatmap_from_file "my_file.osu" with
	| Ok beatmap -> ...
	| Error exn -> Logging.Error("Error while trying to do ...: ...")
```

Always use a `.` when subscripting arrays, even where it would parse without  
This intentionally ignores [the recommendation here](https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/arrays#access-elements)  
Bad: `my_array[i]`, `my_2d_array[i].[j]`  
Good: `my_array.[i]`, `my_2d_array.[i].[j]`  
Things that don't parse: `my_2d_array[i][j]`, `my_function()[0]` hence the consistent rule everywhere