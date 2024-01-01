namespace Prelude.Data.Charts

open System
open System.IO
open System.IO.Compression
open System.Collections.Concurrent
open Percyqaz.Json
open Percyqaz.Common
open Prelude.Common
open Prelude.Charts.Conversions
open Prelude.Charts.Tools.Patterns
open Prelude.Data.Charts.Caching

open Collections

module Library =

    type Patterns = ConcurrentDictionary<string, Patterns.PatternReport>

    // todo: function to load these as it's unpredictable having it load static like this
    let cache: Cache = Cache.from_path (get_game_folder "Songs")

    let collections =
        let cs: Collections =
            load_important_json_file "Collections" (Path.Combine(get_game_folder "Data", "collections.json")) false

        Logging.Info(
            sprintf
                "Loaded chart library of %i charts, %i folders, %i playlists"
                cache.Entries.Count
                cs.Folders.Count
                cs.Playlists.Count
        )

        cs

    let patterns: Patterns =
        let path = Path.Combine(get_game_folder "Data", "patterns.json")
        if File.GetLastWriteTimeUtc(path) > DateTime.Parse("12/12/2023") then
            load_important_json_file "Patterns" (Path.Combine(get_game_folder "Data", "patterns.json")) false
        else
            Logging.Info("Pattern analysis has updated, you will need to cache patterns again")
            Patterns()

    // ---- Basic data layer stuff ----

    let save () =
        Cache.save cache
        save_important_json_file (Path.Combine(get_game_folder "Data", "collections.json")) collections
        save_important_json_file (Path.Combine(get_game_folder "Data", "patterns.json")) patterns

    // ---- Importing charts to library ----

    module Imports =

        let OSU_SONG_FOLDER =
            Path.Combine(Environment.GetFolderPath Environment.SpecialFolder.LocalApplicationData, "osu!", "Songs")

        let STEPMANIA_PACK_FOLDER =
            Path.Combine(Path.GetPathRoot Environment.CurrentDirectory, "Games", "Stepmania 5", "Songs")

        let ETTERNA_PACK_FOLDER =
            Path.Combine(Path.GetPathRoot Environment.CurrentDirectory, "Games", "Etterna", "Songs")

        [<Json.AutoCodec>]
        type MountedChartSourceType =
            | Pack of name: string
            | Library

        [<Json.AutoCodec>]
        type MountedChartSource =
            {
                SourceFolder: string
                mutable LastImported: DateTime
                Type: MountedChartSourceType
                ImportOnStartup: bool
            }
            static member Pack(name: string, path: string) =
                {
                    SourceFolder = path
                    LastImported = DateTime.UnixEpoch
                    Type = Pack name
                    ImportOnStartup = false
                }

            static member Library(path: string) =
                {
                    SourceFolder = path
                    LastImported = DateTime.UnixEpoch
                    Type = Library
                    ImportOnStartup = false
                }

        let convert_song_folder =
            { new Async.Service<string * ConversionOptions, unit>() with
                override this.Handle((path, config)) =
                    async {
                        Directory.EnumerateFiles path
                        |> Seq.collect (
                            function
                            | ChartFile _ as file ->
                                try
                                    let action = { Config = config; Source = file }
                                    convert_chart_file action
                                with err ->
                                    Logging.Error("Exception while converting file: " + file, err)
                                    []
                            | _ -> []
                        )
                        |> List.ofSeq
                        |> fun charts -> Cache.add_new config.PackName charts cache
                    }
            }

        let convert_pack_folder =
            { new Async.Service<string * ConversionOptions, unit>() with
                override this.Handle((path, config)) =
                    async {
                        for songFolder in
                            (Directory.EnumerateDirectories path
                             |> match config.ChangedAfter with
                                | None -> id
                                | Some timestamp ->
                                    Seq.filter (fun path -> Directory.GetLastWriteTime path >= timestamp)) do
                            do! convert_song_folder.RequestAsync(songFolder, config)
                    }
            }

        let import_mounted_source =
            { new Async.Service<MountedChartSource, unit>() with
                override this.Handle(source) =
                    async {
                        match source.Type with
                        | Pack packname ->
                            let config =
                                { ConversionOptions.Default with
                                    MoveAssets = false
                                    ChangedAfter = Some source.LastImported
                                    PackName = packname
                                }

                            do! convert_pack_folder.RequestAsync(source.SourceFolder, config)
                            source.LastImported <- DateTime.UtcNow
                        | Library ->
                            for packFolder in
                                Directory.EnumerateDirectories source.SourceFolder
                                |> Seq.filter (fun path -> Directory.GetLastWriteTime path >= source.LastImported) do
                                do!
                                    convert_pack_folder.RequestAsync(
                                        packFolder,
                                        { ConversionOptions.Default with
                                            MoveAssets = false
                                            ChangedAfter = Some source.LastImported
                                            PackName = Path.GetFileName packFolder
                                        }
                                    )

                            source.LastImported <- DateTime.UtcNow
                    }
            }

        let convert_stepmania_pack_zip =
            { new Async.Service<string * int, bool>() with
                override this.Handle((path, pack_id)) =
                    async {
                        let dir = Path.ChangeExtension(path, null)

                        if
                            Directory.Exists(dir)
                            && Directory.EnumerateFileSystemEntries(dir) |> Seq.isEmpty |> not
                        then
                            Logging.Error(sprintf "Can't extract zip to %s because that folder exists already" dir)
                            return false
                        else
                            ZipFile.ExtractToDirectory(path, dir)

                            match dir with
                            | FolderOfPacks ->
                                for packFolder in Directory.EnumerateDirectories dir do
                                    do!
                                        convert_pack_folder.RequestAsync(
                                            packFolder,
                                            { ConversionOptions.Default with
                                                StepmaniaPackId = Some pack_id
                                                PackName = Path.GetFileName packFolder
                                                MoveAssets = true
                                            }
                                        )

                                Directory.Delete(dir, true)
                                return true
                            | _ ->
                                Logging.Warn(
                                    sprintf
                                        "%s: Extracted zip does not match the usual structure for a StepMania pack"
                                        dir
                                )

                                Directory.Delete(dir, true)
                                return false
                    }
            }

        let auto_convert =
            { new Async.Service<string * bool, bool>() with
                override this.Handle((path, move_assets)) =
                    async {
                        match File.GetAttributes path &&& FileAttributes.Directory |> int with
                        | 0 ->
                            match path with
                            | ChartFile ext ->
                                do!
                                    convert_song_folder.RequestAsync(
                                        Path.GetDirectoryName path,
                                        { ConversionOptions.Default with
                                            PackName = if ext = ".osu" then "osu!" else "Singles"
                                        }
                                    )

                                return true
                            | ChartArchive ->
                                let dir = Path.ChangeExtension(path, null)

                                if
                                    Directory.Exists(dir)
                                    && Directory.EnumerateFileSystemEntries(dir) |> Seq.isEmpty |> not
                                then
                                    Logging.Error(
                                        sprintf "Can't extract zip to %s because that folder exists already" dir
                                    )

                                    return false
                                else
                                    ZipFile.ExtractToDirectory(path, dir)
                                    this.Request((dir, true), (fun _ -> Directory.Delete(dir, true)))
                                    return true
                            | _ ->
                                Logging.Warn(sprintf "%s: Unrecognised file for import" path)
                                return false
                        | _ ->
                            match path with
                            | SongFolder ext ->
                                do!
                                    convert_song_folder.RequestAsync(
                                        path,
                                        { ConversionOptions.Default with
                                            MoveAssets = move_assets
                                            PackName = if ext = ".osu" then "osu!" else "Singles"
                                        }
                                    )

                                return true
                            | PackFolder ->
                                let packname =
                                    match Path.GetFileName path with
                                    | "Songs" ->
                                        if path |> Path.GetDirectoryName |> Path.GetFileName = "osu!" then
                                            "osu!"
                                        else
                                            "Songs"
                                    | s -> s

                                do!
                                    convert_pack_folder.RequestAsync(
                                        path,
                                        { ConversionOptions.Default with
                                            PackName = packname
                                            MoveAssets = move_assets
                                        }
                                    )

                                return true
                            | FolderOfPacks ->
                                for packFolder in Directory.EnumerateDirectories path do
                                    do!
                                        convert_pack_folder.RequestAsync(
                                            packFolder,
                                            { ConversionOptions.Default with
                                                PackName = Path.GetFileName packFolder
                                                MoveAssets = move_assets
                                            }
                                        )

                                return true
                            | _ ->
                                Logging.Warn(sprintf "%s: No importable folder structure detected" path)
                                return false
                    }
            }

    let cache_patterns =
        { new Async.Service<unit, unit>() with
            override this.Handle(()) =
                async {
                    for entry in cache.Entries.Values do
                        if not (patterns.ContainsKey entry.Hash) then
                            match Cache.load entry cache with
                            | Some c ->
                                patterns.[entry.Hash] <-
                                    Prelude.Charts.Tools.Patterns.Patterns.generate_pattern_report (1.0f, c)
                            | None -> ()
                }
        }
