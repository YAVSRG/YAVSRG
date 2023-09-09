namespace Interlude.Features.Printerlude

open System.IO
open Percyqaz.Shell
open Percyqaz.Shell.Shell
open Percyqaz.Common
open Prelude.Common
open Prelude.Data.Charts
open Prelude.Data.Charts.Caching
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
                .WithIOCommand("import_noteskin", "Import a noteskin from an existing source", "path", "keymodes", 
                    fun io path keymodes -> if not (Content.Noteskins.tryImport path keymodes) then io.WriteLine("Nothing found to import"))

    module private Utils =

        open Prelude.Charts.Formats
        open Prelude.Charts.Tools.Patterns
        open System.IO.Compression

        let mutable cmp = None
        let cmp_1() =
            match Gameplay.Chart.current with
            | None -> failwith "Select a chart"
            | Some c -> cmp <- Some c
        let cmp_2() =
            match cmp with
            | None -> failwith "Use cmp_1 first"
            | Some cmp ->
            match Gameplay.Chart.current with
            | None -> failwith "Select a chart"
            | Some c -> Interlude.Chart.diff cmp c

        let show_version (io: IOContext) =
            io.WriteLine(sprintf "You are running %s" Utils.version)
            io.WriteLine(sprintf "The latest version online is %s" Utils.AutoUpdate.latestVersionName)

        let export_osz() =
            match Gameplay.Chart.current with
            | None -> failwith "No chart to export"
            | Some c ->
                // todo: move into prelude
                let beatmap = Conversions.Interlude.toOsu c
                let exportName = ``osu!``.getBeatmapFilename beatmap
                let path = getDataPath "Exports"
                let beatmapFile = Path.Combine(path, exportName)
                ``osu!``.saveBeatmapFile beatmapFile beatmap
                let target_audio_file =
                    match c.Header.AudioFile with 
                    | Interlude.Relative s -> Some <| Path.Combine(path, s)
                    | Interlude.Absolute s -> Some <| Path.Combine(path, Path.GetFileName s)
                    | Interlude.Asset s -> Some <| Path.Combine(path, "audio.mp3")
                    | Interlude.Missing -> None
                
                let target_bg_file =
                    match c.Header.BackgroundFile with 
                    | Interlude.Relative s -> Some <| Path.Combine(path, s)
                    | Interlude.Absolute s -> Some <| Path.Combine(path, Path.GetFileName s)
                    | Interlude.Asset s -> Some <| Path.Combine(path, "bg.png")
                    | Interlude.Missing -> None
                try
                    match target_audio_file with Some p -> File.Copy ((Cache.audio_path c Library.cache).Value, p, true) | _ -> ()
                    match target_bg_file with Some p -> File.Copy ((Cache.background_path c Library.cache).Value, p, true) | _ -> ()
                with err -> printfn "%O" err

                use fs = File.Create(Path.Combine(path, Path.ChangeExtension(exportName, ".osz")))
                use archive = new ZipArchive(fs, ZipArchiveMode.Create, true)
                archive.CreateEntryFromFile(beatmapFile, Path.GetFileName beatmapFile) |> ignore
                match target_audio_file with Some p -> archive.CreateEntryFromFile(p, Path.GetFileName p) |> ignore | _ -> ()
                match target_bg_file with Some p -> archive.CreateEntryFromFile(p, Path.GetFileName p) |> ignore | _ -> ()
                Logging.Info "Exported."
                try
                    match target_audio_file with Some p -> File.Delete p | _ -> ()
                    match target_bg_file with Some p -> File.Delete p | _ -> ()
                    File.Delete beatmapFile
                    Logging.Info "Cleaned up."
                with err -> Logging.Error ("Error while cleaning up after export", err)

        let fix_personal_bests() = ()
        
        let register_commands (ctx: ShellContext) = 
            ctx
                .WithCommand("exit", "Exits the game", fun () -> UI.Screen.exit <- true)
                .WithCommand("clear", "Clears the terminal", Terminal.Log.clear)
                .WithCommand("export_osz", "Export current chart as osz", export_osz)
                .WithCommand("fix_personal_bests", "Fix personal best display values", fix_personal_bests)
                .WithIOCommand("local_server", "Switch to local development server", "flag", 
                    fun (io: IOContext) (b: bool) -> 
                        Online.Network.credentials.Host <- (if b then "localhost" else "online.yavsrg.net")
                        Online.Network.credentials.Api <- (if b then "localhost" else "api.yavsrg.net")
                        io.WriteLine("Restart your game to apply server change."))
                .WithCommand("cmp_1", "Select chart to compare against", cmp_1)
                .WithCommand("cmp_2", "Compare current chart to selected chart", cmp_2)

        let register_ipc_commands (ctx: ShellContext) =
            ctx
                .WithIOCommand("version", "Shows info about the current game version", show_version)

    let private ms = new MemoryStream()
    let private context_output = new StreamReader(ms)
    let private context_writer = new StreamWriter(ms)

    let io = { In = stdin; Out = context_writer }

    ctx <-
        ShellContext.Empty
        |> Utils.register_ipc_commands
        |> Utils.register_commands
        |> Themes.register_commands

    let exec(s: string) =
        let msPos = ms.Position
        ctx.Evaluate io s
        context_writer.Flush()
        ms.Position <- msPos
        Terminal.add_message (context_output.ReadToEnd())

    let mutable ipc_shutdown_token : System.Threading.CancellationTokenSource option = None

    let ipc_commands = 
        ShellContext.Empty
        |> Utils.register_ipc_commands

    let init(instance: int) =
        Terminal.exec_command <- exec
        if instance = 0 then ipc_shutdown_token <- Some (IPC.start_server_thread "Interlude" ipc_commands)

    let shutdown() = ipc_shutdown_token |> Option.iter (fun token -> token.Cancel())