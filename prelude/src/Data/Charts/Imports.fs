namespace Prelude.Data.Charts

open System
open System.IO
open System.IO.Compression
open Percyqaz.Data
open Percyqaz.Common
open Prelude.Charts.Conversions
open Prelude.Data.Charts.Caching

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
        { new Async.Service<string * ConversionOptions * Library, unit>() with
            override this.Handle((path, config, { Cache = cache })) =
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
                    // todo: also save the patterns to the pattern store
                }
        }

    let convert_pack_folder =
        { new Async.Service<string * ConversionOptions * Library, unit>() with
            override this.Handle((path, config, library)) =
                async {
                    for song_folder in
                        (
                            Directory.EnumerateDirectories path
                            |> 
                                match config.ChangedAfter with
                                | None -> id
                                | Some timestamp -> Seq.filter (fun path -> Directory.GetLastWriteTime path >= timestamp)
                        )
                        do
                        do! convert_song_folder.RequestAsync(song_folder, config, library)
                }
        }

    let import_mounted_source =
        { new Async.Service<MountedChartSource * Library, unit>() with
            override this.Handle((source, library)) =
                async {
                    match source.Type with
                    | Pack packname ->
                        let config =
                            { ConversionOptions.Default with
                                MoveAssets = false
                                ChangedAfter = Some source.LastImported
                                PackName = packname
                            }

                        do! convert_pack_folder.RequestAsync(source.SourceFolder, config, library)
                        source.LastImported <- DateTime.UtcNow
                    | Library ->
                        for pack_folder in
                            Directory.EnumerateDirectories source.SourceFolder
                            |> Seq.filter (fun path -> Directory.GetLastWriteTime path >= source.LastImported) do
                            do!
                                convert_pack_folder.RequestAsync(
                                    pack_folder,
                                    { ConversionOptions.Default with
                                        MoveAssets = false
                                        ChangedAfter = Some source.LastImported
                                        PackName = Path.GetFileName pack_folder
                                    },
                                    library
                                )

                        source.LastImported <- DateTime.UtcNow
                }
        }

    let convert_stepmania_pack_zip =
        { new Async.Service<string * int * Library, bool>() with
            override this.Handle((path, pack_id, library)) =
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
                            for pack_folder in Directory.EnumerateDirectories dir do
                                do!
                                    convert_pack_folder.RequestAsync(
                                        pack_folder,
                                        { ConversionOptions.Default with
                                            StepmaniaPackId = Some pack_id
                                            PackName = Path.GetFileName pack_folder
                                            MoveAssets = true
                                        },
                                        library
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
        { new Async.Service<string * bool * Library, bool>() with
            override this.Handle((path, move_assets, library)) =
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
                                    },
                                    library
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
                                this.Request((dir, true, library), (fun _ -> Directory.Delete(dir, true)))
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
                                    },
                                    library
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
                                    },
                                    library
                                )

                            return true
                        | FolderOfPacks ->
                            for pack_folder in Directory.EnumerateDirectories path do
                                do!
                                    convert_pack_folder.RequestAsync(
                                        pack_folder,
                                        { ConversionOptions.Default with
                                            PackName = Path.GetFileName pack_folder
                                            MoveAssets = move_assets
                                        },
                                        library
                                    )

                            return true
                        | _ ->
                            Logging.Warn(sprintf "%s: No importable folder structure detected" path)
                            return false
                }
        }