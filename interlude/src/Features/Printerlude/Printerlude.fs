namespace Interlude.Features.Printerlude

open System.IO
open Percyqaz.Shell
open Percyqaz.Shell.Shell
open Percyqaz.Common
open Prelude.Common
open Interlude
open Interlude.Features

module Printerlude =

    let mutable private ctx : ShellContext = Unchecked.defaultof<_>

    module private Themes =

        let register_commands (ctx: ShellContext) =
            ctx
                .WithCommand("themes_reload", "Reload the current theme and noteskin", fun () -> Content.Themes.load(); Content.Noteskins.load())
                .WithCommand("noteskin_stitch", "Stitch a noteskin texture", "id", fun id -> Content.Noteskins.Current.instance.StitchTexture id)
                .WithCommand("noteskin_split", "Split a noteskin texture", "id", fun id -> Content.Noteskins.Current.instance.SplitTexture id)
                .WithCommand("import_noteskin", "Import a noteskin from an existing source", "path", "keymodes", 
                    fun path keymodes -> if not (Content.Noteskins.tryImport path keymodes) then ctx.WriteLine("Nothing found to import"))

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

                for (p, bpm) in data.Keys |> Seq.sortByDescending(fun k -> data.[k].TotalTime) do
                    let d = data.[(p, bpm)]
                    let percent = d.TotalTime / duration * 100.0f

                    let category =
                        if d.Marathons.Count > 0 then "Stamina"
                        elif d.Sprints.Count > 1 then "Sprints"
                        elif d.Sprints.Count = 1 then "Sprint"
                        elif d.Runs.Count > 1 then "Runs"
                        elif d.Runs.Count = 1 then "Run"
                        elif d.Bursts.Count > 1 then "Bursts"
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
        
        let register_commands (ctx: ShellContext) = 
            ctx
                .WithCommand("version", "Shows info about the current game version", show_version)
                .WithCommand("exit", "Exits the game", fun () -> UI.Screen.exit <- true)
                .WithCommand("clear", "Clears the terminal", Terminal.Log.clear)
                .WithCommand("export_osz", "Export current chart as osz", export_osz)
                .WithCommand("patterns", "Experimental", analyse_patterns)
                .WithCommand("local_server", "Switch to local development server", "flag", fun b -> Online.Network.credentials.Host <- (if b then "localhost" else "online.yavsrg.net"); ctx.WriteLine("Restart your game to apply server change."))

    let private ms = new MemoryStream()
    let private context_output = new StreamReader(ms)
    let private context_writer = new StreamWriter(ms)

    ctx <-
        { ShellContext.Empty with IO = { In = stdin; Out = context_writer } }
        |> Themes.register_commands
        |> Utils.register_commands

    let exec(s: string) =
        let msPos = ms.Position
        ctx.Evaluate s
        context_writer.Flush()
        ms.Position <- msPos
        Terminal.add_message (context_output.ReadToEnd())

    let init() = Terminal.exec_command <- exec