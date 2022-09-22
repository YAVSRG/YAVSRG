namespace Interlude.Features.Printerlude

open System.IO
open Percyqaz.Shell
open Percyqaz.Shell.Shell
open Percyqaz.Common
open Prelude.Common
open Prelude.Data.Tables
open Interlude
open Interlude.Features

module Printerlude =

    let mutable private ctx : Context = Unchecked.defaultof<_>

    module private Tables = 
        
        let register_commands (ctx: Context) =
            ctx
                .WithCommand("table_load", Command.create "Loads a table from file" ["filename"]
                    ( Impl.Create(Types.str, Gameplay.selectTable) ))

                .WithCommand("table_save", Command.create "Creates a new table" []
                    ( Impl.Create Table.save ))

                .WithCommand("table_create", Command.create "Creates a new table" ["name"; "filename"]
                    ( Impl.Create(Types.str, Types.str, fun name filename -> Table.create(name, filename)) ))

                .WithCommand("table_create_level", Command.create "Creates a new level in the current table" ["level_name"]
                    ( Impl.Create(Types.str, fun levelid -> Table.current.Value.CreateLevel levelid) ))

                .WithCommand("table_remove_level", Command.create "Deletes a level in the current table" ["level_name"]
                    ( Impl.Create(Types.str, fun levelid -> Table.current.Value.RemoveLevel levelid) ))
                    
                .WithCommand("table_rename_level", Command.create "Renames a level in the current table" ["old_level_name"; "new_level_name"]
                    ( Impl.Create(Types.str, Types.str, fun oldname newname -> Table.current.Value.RenameLevel(oldname, newname)) ))

                .WithCommand("table_add_chart", Command.create "Adds the current chart to the current table" ["level_name"; "chart_id"]
                    ( Impl.Create(Types.str, Types.str, 
                        fun levelid chartid -> Table.current.Value.AddChart(levelid, chartid, Gameplay.Chart.cacheInfo.Value)) ))

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
        open System.IO.Compression

        let show_version() =
            ctx.WriteLine(sprintf "You are running %s" Utils.version)
            ctx.WriteLine(sprintf "The latest version online is %s" Utils.AutoUpdate.latestVersionName)

        let fft() =
            match Gameplay.Chart.current with
            | Some c -> 
                for (v, v2) in Prelude.Gameplay.FFT.ffts c.Notes do
                    ctx.WriteLine(sprintf "%.2f :: %.3f" v v2)
            | None -> ()

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
                    File.Copy (c.AudioPath, audioFile)
                    File.Copy (c.BackgroundPath, bgFile)
                with err -> printfn "%O" err
                use fs = File.Create(Path.Combine(path, Path.ChangeExtension(exportName, ".osz")))
                use archive = new ZipArchive(fs, ZipArchiveMode.Create, true)
                archive.CreateEntryFromFile(beatmapFile, Path.GetFileName beatmapFile) |> ignore
                archive.CreateEntryFromFile(audioFile, Path.GetFileName audioFile) |> ignore
                archive.CreateEntryFromFile(bgFile, Path.GetFileName bgFile) |> ignore
                Logging.Info "Exported."
        
        let register_commands (ctx: Context) = 
            ctx
                .WithCommand("version", Command.create "Shows info about the current game version" [] (Impl.Create show_version))
                .WithCommand("exit", Command.create "Exits the game" [] (Impl.Create (fun () -> UI.Screen.exit <- true)))
                .WithCommand("clear", Command.create "Clears the terminal" [] (Impl.Create Terminal.Log.clear))
                .WithCommand("export_osz", Command.create "Export current file as osz" [] (Impl.Create export_osz))
                .WithCommand("fft", Command.create "Experimental" [] (Impl.Create fft))

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