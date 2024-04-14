namespace Interlude.Tools

open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic
open Percyqaz.Shell
open Prelude.Gameplay.Mods
open Interlude.Tools.Utils

module Check =

    let rec private walk_fs_files (dir: string) : (string * string) seq =
        seq {
            for file in Directory.GetFiles(dir) do
                if Path.GetExtension(file).ToLower() = ".fs" then
                    yield file, File.ReadAllText file

            for dir in Directory.GetDirectories(dir) do
                let name = Path.GetFileName dir

                if name <> "bin" && name <> "obj" then
                    yield! walk_fs_files dir
        }

    let private load_locale (file: string) =
        let mapping = new Dictionary<string, string>()
        let path = Path.Combine(INTERLUDE_SOURCE_PATH, "Locale", file + ".txt")
        let lines = File.ReadAllLines path

        Array.iter
            (fun (l: string) ->
                let s: string[] = l.Split([| '=' |], 2)
                mapping.Add(s.[0], s.[1].Replace("\\n", "\n"))
            )
            lines

        mapping

    let check_linecounts() =
        for filename, file_contents in walk_fs_files YAVSRG_PATH do
            let lines = file_contents.Split('\n').Length
            if lines > 300 then printfn "%s has %i lines" filename lines

    let check_locale (file: string) (cli_fix_issues: bool) =
        let locale = load_locale file

        let new_locale = Dictionary(locale)

        let mutable sources = Map.empty<string, string>
        let mutable found = Set.empty<string>

        let find x source =
            if not (found.Contains x) then
                found <- Set.add x found
                sources <- Map.add x source sources

        let matches (reg: string) (input: string) =
            seq {
                for m in Regex(reg.Trim()).Matches(input) do
                    yield m.Index, (m.Groups.[1].Value)
            }

        for filename, file_contents in walk_fs_files INTERLUDE_SOURCE_PATH do

            for position, m in matches """ [^%]%"([a-z\-_\.]*)" """ file_contents do
                find m (sprintf "%s (position %i)" filename position)

            for position, m in matches """ %> "([a-z\-_\.]*)" """ file_contents do
                find m (sprintf "%s (position %i)" filename position)

            for position, m in matches """ PageSetting\(\s*"([a-z\-_\.]*[^\.])" """ file_contents do
                find (sprintf "%s.name" m) (sprintf "%s (position %i)" filename position)

            for position, m in matches """ PageButton\(\s*"([a-z\-_\.]*)" """ file_contents do
                find (sprintf "%s.name" m) (sprintf "%s (position %i)" filename position)

            for position, m in matches """ PageTextEntry\(\s*"([a-z\-_\.]*)" """ file_contents do
                find (sprintf "%s.name" m) (sprintf "%s (position %i)" filename position)

            for position, m in matches """ PageButton\s*.Once\(\s*"([a-z\-_\.]*)" """ file_contents do
                find (sprintf "%s.name" m) (sprintf "%s (position %i)" filename position)

            for position, m in matches """ Tooltip\s*.Info\("([a-z\-_\.]*)" """ file_contents do
                find (sprintf "%s.name" m) (sprintf "%s (position %i)" filename position)
                find (sprintf "%s.tooltip" m) (sprintf "%s (position %i)" filename position)

            for position, m in matches """ localise\s*"([a-z\-_\.]*)" """ file_contents do
                find m (sprintf "%s (position %i)" filename position)

            for position, m in matches """ localiseWith\s*\[.*\]\s*"([a-z\-_\.]*)" """ file_contents do
                find m (sprintf "%s (position %i)" filename position)

            for m in
                Seq.append
                    [ "exit"; "select"; "up"; "down"; "left"; "right" ]
                    (matches """ Hotkeys.register "([a-z0-9\-_\.]*)" """ file_contents |> Seq.map snd) do
                find (sprintf "hotkeys.%s.name" m) "Hotkeys"
                find (sprintf "hotkeys.%s.tooltip" m) "Hotkeys"

            for m in [ "auto"; "pacemaker" ] do
                find (sprintf "mod.%s.name" m) "Mods"
                find (sprintf "mod.%s.desc" m) "Mods"
            for m in Mods.AVAILABLE_MODS.Keys do
                find (sprintf "mod.%s.name" m) "Mods"
                find (sprintf "mod.%s.desc" m) "Mods"
                if Mods.AVAILABLE_MODS.[m].RandomSeed |> not then
                    for i in 1 .. Mods.AVAILABLE_MODS.[m].States - 1 do
                        find (sprintf "mod.%s.%i.name" m i) "Mods"
                        find (sprintf "mod.%s.%i.desc" m i) "Mods"

            for i = 0 to 9 do
                find (sprintf "noteskins.edit.notecolors.chord.%i" i) "Note color tooltips"
                find (sprintf "noteskins.edit.notecolors.column.%i" i) "Note color tooltips"

                if i < 9 then
                    find (sprintf "noteskins.edit.notecolors.ddr.%i" i) "Note color tooltips"

            for m in Prelude.Data.Library.Sorting.grouping_modes.Keys do
                find (sprintf "levelselect.groupby.%s" m) "Level select grouping"

            for m in Prelude.Data.Library.Sorting.sorting_modes.Keys do
                find (sprintf "levelselect.sortby.%s" m) "Level select sorting"

        for m in found |> Seq.sort do
            if locale.ContainsKey m then
                locale.Remove m |> ignore
            else
                printfn "'%s' is missing!\nFound in %s" m sources.[m]
                if cli_fix_issues then
                    printf "Enter a value: (Leave blank to skip, enter a space to deliberately leave empty)\n> "
                    match Console.ReadLine() with
                    | "" -> ()
                    | new_value -> 
                        new_locale.Add(m, new_value.Trim())

        for m in locale.Keys do
            printfn "Unused locale key: %s" m
            if cli_fix_issues then
                printfn "Remove it? [Y/N]"
                match Console.ReadLine().ToLower() with
                | "y" -> new_locale.Remove(m) |> ignore
                | _ -> ()

        if cli_fix_issues then
            new_locale.Keys 
            |> Seq.sort 
            |> Seq.map (fun key -> sprintf "%s=%s" key (new_locale.[key].Replace("\n", "\\n")))
            |> fun contents -> File.WriteAllLines(Path.Combine(INTERLUDE_SOURCE_PATH, "Locale", file + ".txt"), contents)

    let locale_rename (file: string) (before: string) (after: string) =
        let locale = load_locale file
        failwith "nyi"

    let register (ctx: ShellContext) : ShellContext =
        ctx
            .WithCommand("check_locale", "Check locale for mistakes", (fun () -> check_locale "en_GB" false))
            .WithCommand("fix_locale", "Tool to automatically add locale keys", (fun () -> check_locale "en_GB" true))
            .WithCommand("check_linecounts", "Check for particularly large source code files", (fun () -> check_linecounts()))
