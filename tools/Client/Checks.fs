namespace YAVSRG.CLI

open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic
open Prelude.Mods
open YAVSRG.CLI.Utils

module Check =

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

    let simple_view (file_contents: string) =
        let regex = Regex("[\t ]*(module .+? =|type .+? =|let .+? =|member .+? =|override .+? =)")
        let span = file_contents.AsSpan()
        for m in regex.EnumerateMatches span do
            let match_span = span.Slice(m.Index, m.Length)
            printfn "%s" (String match_span)

    let simple_view_all () =
        for filename, file_contents in walk_fs_files YAVSRG_PATH do
            printfn "%s\n====\n" filename
            simple_view file_contents

    let check_linecounts () =
        let mutable loc = 0
        for filename, file_contents in walk_fs_files YAVSRG_PATH do
            let lines = file_contents.Split('\n').Length

            if lines > 300 then
                printfn "%s has %i lines" filename lines
            loc <- loc + lines
        printfn "total %i lines of f#" loc

    let locale_check (file: string) (cli_fix_issues: bool) =
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

            for position, m in matches """ Help\s*.Info\("([a-z\-_\.]*)" """ file_contents do
                find m (sprintf "%s (position %i)" filename position)
                find (sprintf "%s.tooltip" m) (sprintf "%s (position %i)" filename position)

            for position, m in matches """ localise\s*"([a-z\-_\.]*)" """ file_contents do
                find m (sprintf "%s (position %i)" filename position)

            for position, m in matches """ localiseWith\s*\[.*\]\s*"([a-z\-_\.]*)" """ file_contents do
                find m (sprintf "%s (position %i)" filename position)

            for m in
                Seq.append
                    [ "exit"; "select"; "up"; "down"; "left"; "right" ]
                    (matches """ Hotkeys.register "([a-z0-9\-_\.]*)" """ file_contents |> Seq.map snd) do
                find (sprintf "hotkeys.%s" m) "Hotkeys"
                find (sprintf "hotkeys.%s.tooltip" m) "Hotkeys"

            for m in [ "auto"; "pacemaker" ] do
                find (sprintf "mod.%s" m) "Mods"
                find (sprintf "mod.%s.desc" m) "Mods"

            for m in Mods.AVAILABLE_MODS.Keys do
                find (sprintf "mod.%s" m) "Mods"
                find (sprintf "mod.%s.desc" m) "Mods"

                if Mods.AVAILABLE_MODS.[m].RandomSeed |> not then
                    for i in 1 .. Mods.AVAILABLE_MODS.[m].States - 1 do
                        find (sprintf "mod.%s.%i" m i) "Mods"
                        find (sprintf "mod.%s.%i.desc" m i) "Mods"

            for i = 0 to 9 do
                find (sprintf "noteskin.notecolors.chord.%i" i) "Note color tooltips"
                find (sprintf "noteskin.notecolors.column.%i" i) "Note color tooltips"

                if i < 9 then
                    find (sprintf "noteskin.notecolors.ddr.%i" i) "Note color tooltips"

            for m in Prelude.Data.Library.Grouping.modes.Keys do
                find (sprintf "levelselect.groupby.%s" m) "Level select grouping"

            for m in Prelude.Data.Library.Sorting.modes.Keys do
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
                    | new_value -> new_locale.Add(m, new_value.Trim())

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
            |> fun contents ->
                File.WriteAllLines(Path.Combine(INTERLUDE_SOURCE_PATH, "Locale", file + ".txt"), contents)

    let locale_rename (file: string) (before: string) =
        let locale = load_locale file

        let to_rename =
            locale.Keys
            |> Seq.where (fun key -> key.StartsWith before)
            |> Array.ofSeq

        printfn "This will rename %i keys:" to_rename.Length

        for key in to_rename |> Array.truncate 5 do
            printfn "  %s" key

        if to_rename.Length > 5 then
            printfn "  ..."

        printf "Rename to > "
        let after = Console.ReadLine()

        let replaces (reg: string) (fmt: Printf.StringFormat<string -> string -> string>) (input: string) : string =
            let mutable result = input

            for m in Regex(reg.Trim()).Matches(input) do
                if m.Groups.[2].Value.StartsWith(before) then
                    let replace_from = m.Groups.[0].Value

                    let replace_with =
                        sprintf fmt m.Groups.[1].Value (after + m.Groups.[2].Value.Substring(before.Length))

                    result <- result.Replace(replace_from, replace_with)

            result

        for filename, file_contents in walk_fs_files INTERLUDE_SOURCE_PATH do
            let replaced_contents =
                file_contents
                |> replaces """ ([^%])%"([a-z\-_\.]*)" """ "%s%%\"%s\""
                |> replaces """ (%> )"([a-z\-_\.]*)" """ "%s\"%s\""
                |> replaces """ Help(\s*).Info\("([a-z\-_\.]*)" """ "Help%s.Info(\"%s\""

            if replaced_contents <> file_contents then
                File.WriteAllText(filename, replaced_contents)

        for key in to_rename do
            locale.[after + key.Substring(before.Length)] <- locale.[key]
            locale.Remove(key) |> ignore

        locale.Keys
        |> Seq.sort
        |> Seq.map (fun key -> sprintf "%s=%s" key (locale.[key].Replace("\n", "\\n")))
        |> fun contents -> File.WriteAllLines(Path.Combine(INTERLUDE_SOURCE_PATH, "Locale", file + ".txt"), contents)

    let format_all_code () = exec "fantomas" "."