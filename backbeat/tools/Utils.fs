﻿namespace Backbeat

open Percyqaz.Common
open Percyqaz.Json
open Prelude.Common

module Utils =

    open System.IO
    open System.Runtime.CompilerServices
    open System.Runtime.InteropServices

    type PathHelper() =
        static member Path([<CallerFilePath; Optional; DefaultParameterValue("")>] path: string) : string =
            Path.Combine(path, "..", "..") |> Path.GetFullPath

    let REPO_PATH = PathHelper.Path()

    let SETTINGS_FILE = Path.Combine(REPO_PATH, "settings.json")

    [<Json.AutoCodec>]
    type Config =
        {
            InterludePath: string
            S3ApiKey: string
            S3ApiKeyID: string
        }
        static member Default =
            {
                InterludePath = "C:/Interlude/dev"
                S3ApiKey = ""
                S3ApiKeyID = ""
            }

    let config: Config =
        match JSON.FromFile SETTINGS_FILE with
        | Ok c -> c
        | Error e ->
            Logging.Error("Error loading settings (using default): ", e)
            Config.Default

    let TABLES_PATH = Path.Combine(REPO_PATH, "tables")
    let ARCHIVE_PATH = Path.Combine(REPO_PATH, "archive")
    let BACKBEAT_CACHE_FOLDER = Path.Combine(ARCHIVE_PATH, "yav")
    let RULESETS_PATH = Path.Combine(REPO_PATH, "rulesets")
    let INTERLUDE_TABLES_PATH = Path.Combine(config.InterludePath, "Data", "Tables")

    let INTERLUDE_COLLECTIONS_FILE =
        Path.Combine(config.InterludePath, "Data", "collections.json")

    let INTERLUDE_SONGS_FOLDER = Path.Combine(config.InterludePath, "Songs")

    let init () =
        JSON.ToFile (SETTINGS_FILE, true) config
        Directory.CreateDirectory TABLES_PATH |> ignore

        Logging.Subscribe(fun (level, main, details) ->
            if level = LoggingLevel.ERROR then
                printfn "%s" details
        )
