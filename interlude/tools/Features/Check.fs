namespace Interlude.Tools

open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic
open Interlude.Tools.Utils
open Percyqaz.Shell

module Check =

    let rec walk_fs_files(dir: string) : string seq =
        seq {
            for file in Directory.GetFiles(dir) do
                yield File.ReadAllText file
            for dir in Directory.GetDirectories(dir) do
                yield! walk_fs_files dir
        }

    let check_locale(name: string) =
        let locale = 
            let mapping = new Dictionary<string, string>()
            let path = Path.Combine (INTERLUDE_PATH, "Locale", name + ".txt")
            let lines = File.ReadAllLines path
            Array.iter(
                fun (l: string) ->
                    let s: string[] = l.Split ([|'='|], 2)
                    mapping.Add (s.[0], s.[1].Replace("\\n","\n"))
                ) lines
            mapping

        let referenced =
            let found = new List<string>()
            let matches (reg: string) (input: string) =
                seq {
                    for m in Regex(reg.Trim()).Matches(input) do
                        yield (m.Groups.[1].Value)
                }
            for file_contents in walk_fs_files INTERLUDE_PATH do

                for m in matches """ L"([a-z\-_\.]*)" """ file_contents do
                    found.Add m

                for m in matches """ PrettySetting\(\s*"([a-z\-_\.]*[^\.])" """ file_contents do
                    found.Add (sprintf "options.%s.name" m)
                    found.Add (sprintf "options.%s.tooltip" m)

                for m in matches """ PrettyButton\(\s*"([a-z\-_\.]*)" """ file_contents do
                    found.Add (sprintf "options.%s.name" m)
                    found.Add (sprintf "options.%s.tooltip" m)

                for m in matches """ CaseSelector\(\s*"([a-z\-_\.]*)" """ file_contents do
                    found.Add (sprintf "options.%s.name" m)
                    found.Add (sprintf "options.%s.tooltip" m)
                    
                for m in matches """ PrettyButton.Once\(\s*"([a-z\-_\.]*)" """ file_contents do
                    found.Add (sprintf "options.%s.name" m)
                    found.Add (sprintf "options.%s.tooltip" m)

                for m in matches """ N\s*"([a-z\-_\.]*)" """ file_contents do
                    found.Add (sprintf "options.%s.name" m)

                for m in matches """ localise\s*"([a-z\-_\.]*)" """ file_contents do
                    found.Add m

                for m in matches """ localiseWith\s*\[.*\]\s*"([a-z\-_\.]*)" """ file_contents do
                    found.Add m

                for m in Seq.append ["exit"; "select"; "up"; "down"; "left"; "right"] (matches """ Hotkeys.register "([a-z\-_\.]*)" """ file_contents) do
                    found.Add (sprintf "options.hotkeys.%s.name" m)
                    found.Add (sprintf "options.hotkeys.%s.tooltip" m)

                for m in Seq.append ["auto"] Prelude.Gameplay.Mods.modList.Keys do
                    found.Add (sprintf "mod.%s.name" m)
                    found.Add (sprintf "mod.%s.desc" m)
            found

        for m in referenced |> Seq.distinct |> Seq.sort do
            if locale.ContainsKey m then locale.Remove m |> ignore
            else printfn "Missing locale key: %s" m

        for m in locale.Keys do
            printfn "Unused locale key: %s" m

    let register(ctx: Context) : Context =
        ctx.WithCommand(
            "check_locale",
            Command.create "Check locale for mistakes" [] (Impl.Create (fun () -> check_locale "en_GB"))
        )