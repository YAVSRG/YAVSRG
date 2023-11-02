namespace Interlude.Tools

open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic
open Interlude.Tools.Utils
open Percyqaz.Shell

module Check =

    let rec walk_fs_files (dir: string) : (string * string) seq =
        seq {
            for file in Directory.GetFiles(dir) do
                if Path.GetExtension(file).ToLower() = ".fs" then
                    yield file, File.ReadAllText file

            for dir in Directory.GetDirectories(dir) do
                let name = Path.GetFileName dir

                if name <> "bin" && name <> "obj" then
                    yield! walk_fs_files dir
        }

    let check_locale (name: string) =
        let locale =
            let mapping = new Dictionary<string, string>()
            let path = Path.Combine(INTERLUDE_SOURCE_PATH, "Locale", name + ".txt")
            let lines = File.ReadAllLines path

            Array.iter
                (fun (l: string) ->
                    let s: string[] = l.Split([| '=' |], 2)
                    mapping.Add(s.[0], s.[1].Replace("\\n", "\n"))
                )
                lines

            mapping

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

            for position, m in matches """ CaseSelector\(\s*"([a-z\-_\.]*)" """ file_contents do
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

            for m in Seq.append [ "auto"; "pacemaker" ] Prelude.Gameplay.Mods.available_mods.Keys do
                find (sprintf "mod.%s.name" m) "Mods"
                find (sprintf "mod.%s.desc" m) "Mods"

            for i = 0 to 9 do
                find (sprintf "noteskins.edit.notecolors.chord.%i" i) "Note color tooltips"
                find (sprintf "noteskins.edit.notecolors.column.%i" i) "Note color tooltips"

                if i < 9 then
                    find (sprintf "noteskins.edit.notecolors.ddr.%i" i) "Note color tooltips"

            for m in Prelude.Data.Charts.Sorting.groupBy.Keys do
                find (sprintf "levelselect.groupby.%s" m) "Level select grouping"

            for m in Prelude.Data.Charts.Sorting.sortBy.Keys do
                find (sprintf "levelselect.sortby.%s" m) "Level select sorting"

        for m in found |> Seq.sort do
            if locale.ContainsKey m then
                locale.Remove m |> ignore
            else
                printfn "Missing locale key: %s -- %s" m sources.[m]

        for m in locale.Keys do
            printfn "Unused locale key: %s" m

    let register (ctx: ShellContext) : ShellContext =
        ctx.WithCommand("check_locale", "Check locale for mistakes", (fun () -> check_locale "en_GB"))
