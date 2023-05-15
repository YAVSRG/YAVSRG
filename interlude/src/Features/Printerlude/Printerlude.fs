namespace Interlude.Features.Printerlude

open System.IO
open Percyqaz.Shell
open Percyqaz.Shell.Shell
open Percyqaz.Common
open Prelude.Common
open Interlude
open Interlude.Features

module Printerlude =

    let mutable private ctx : Context = Unchecked.defaultof<_>

    module private Tables = 
        
        let register_commands (ctx: Context) =
            ctx
                .WithCommand("enable_table_editing", Command.create "Enables editing of local tables" []
                    ( Impl.Create (fun () -> Interlude.Options.options.EnableTableEdit.Value <- true) ))

                .WithCommand("disable_table_editing", Command.create "Disables editing of local tables" []
                    ( Impl.Create (fun () -> Interlude.Options.options.EnableTableEdit.Value <- false) ))

    module private Themes =

        let register_commands (ctx: Context) =
            ctx
                .WithCommand("themes_reload", Command.create "Reload the current theme and noteskin" []
                    ( Impl.Create(fun () -> Content.Themes.load(); Content.Noteskins.load()) ))

                .WithCommand("noteskin_stitch", Command.create "Stitch a noteskin texture" ["id"]
                    ( Impl.Create(Types.str, fun id -> Content.Noteskins.Current.instance.StitchTexture id) ))

                .WithCommand("noteskin_split", Command.create "Split a noteskin texture" ["id"]
                    ( Impl.Create(Types.str, fun id -> Content.Noteskins.Current.instance.SplitTexture id) ))
                    
                .WithCommand("import_noteskin", Command.create "Import a noteskin from an existing source" ["path"; "keymodes"]
                    ( Impl.Create(Types.str, (Types.list Types.int), fun path keymodes -> if not (Content.Noteskins.tryImport path keymodes) then ctx.WriteLine("Nothing found to import")) ))

    module private Utils =

        open Prelude.Charts.Formats
        open Prelude.Charts.Tools.Patterns
        open System.IO.Compression

        let show_version() =
            ctx.WriteLine(sprintf "You are running %s" Utils.version)
            ctx.WriteLine(sprintf "The latest version online is %s" Utils.AutoUpdate.latestVersionName)

        let analyse_patterns() =
            match Gameplay.Chart.current with
            | None -> failwith "No chart to analyze"
            | Some c ->
                let duration = Gameplay.Chart.cacheInfo.Value.Length
                let data = 
                    Patterns.analyse c
                    |> Patterns.pattern_locations Gameplay.rate.Value
                    |> Patterns.pattern_breakdown

                for (p, bpm) in data.Keys do
                    let d = data.[(p, bpm)]
                    let percent = d.TotalTime / duration * 100.0f

                    let category =
                        if d.Marathons > 0 then "Stamina"
                        elif d.Sprints > 1 then "Sprints"
                        elif d.Sprints = 1 then "Sprint"
                        elif d.Runs > 1 then "Runs"
                        elif d.Runs = 1 then "Run"
                        elif d.Bursts > 1 then "Bursts"
                        else "Burst"

                    if percent > 1f then ctx.WriteLine(sprintf "%iBPM %s %s: %.2f%%" bpm p category percent)

        let export_osz() =
            match Gameplay.Chart.current with
            | None -> failwith "No chart to export"
            | Some c ->
                let beatmap = Conversions.``Interlude to osu!``.convert c
                let exportName = ``osu!``.getBeatmapFilename beatmap
                let path = getDataPath "Exports"
                let beatmapFile = Path.Combine(path, exportName)
                ``osu!``.saveBeatmapFile beatmapFile beatmap
                let audioFile = Path.Combine(path, Path.GetFileName c.AudioPath)
                let bgFile = Path.Combine(path, Path.GetFileName c.BackgroundPath)
                try
                    File.Copy (c.AudioPath, audioFile, true)
                    File.Copy (c.BackgroundPath, bgFile, true)
                with err -> printfn "%O" err
                use fs = File.Create(Path.Combine(path, Path.ChangeExtension(exportName, ".osz")))
                use archive = new ZipArchive(fs, ZipArchiveMode.Create, true)
                archive.CreateEntryFromFile(beatmapFile, Path.GetFileName beatmapFile) |> ignore
                archive.CreateEntryFromFile(audioFile, Path.GetFileName audioFile) |> ignore
                archive.CreateEntryFromFile(bgFile, Path.GetFileName bgFile) |> ignore
                Logging.Info "Exported."
                try
                    File.Delete(Path.Combine(path, Path.GetFileName c.AudioPath))
                    File.Delete(Path.Combine(path, Path.GetFileName c.BackgroundPath))
                    File.Delete beatmapFile
                    Logging.Info "Cleaned up."
                with err -> Logging.Error ("Error while cleaning up after export", err)
        
        let register_commands (ctx: Context) = 
            ctx
                .WithCommand("version", Command.create "Shows info about the current game version" [] (Impl.Create show_version))
                .WithCommand("exit", Command.create "Exits the game" [] (Impl.Create (fun () -> UI.Screen.exit <- true)))
                .WithCommand("clear", Command.create "Clears the terminal" [] (Impl.Create Terminal.Log.clear))
                .WithCommand("export_osz", Command.create "Export current chart as osz" [] (Impl.Create export_osz))
                .WithCommand("patterns", Command.create "Experimental" [] (Impl.Create analyse_patterns))
                .WithCommand("local_server", Command.create "Switch to local development server" ["flag"] (Impl.Create (Types.bool, fun b -> Online.Network.credentials.Host <- (if b then "localhost" else "online.yavsrg.net"); Logging.Info("Restart your game to apply server change."))))

    let private ms = new MemoryStream()
    let private context_output = new StreamReader(ms)
    let private context_writer = new StreamWriter(ms)

    ctx <-
        { Context.Empty with IO = { In = stdin; Out = context_writer } }
        |> Tables.register_commands
        |> Themes.register_commands
        |> Utils.register_commands

    let exec(s: string) =
        let msPos = ms.Position
        match ctx.Interpret s with
        | Ok new_ctx -> 
            ctx <- new_ctx
            context_writer.Flush()
            ms.Position <- msPos
            Terminal.add_message (context_output.ReadToEnd())
        | ParseFail err -> Terminal.add_message (err.ToString())
        | RunFail err -> Terminal.add_message err.Message

    let init() = Terminal.exec_command <- exec