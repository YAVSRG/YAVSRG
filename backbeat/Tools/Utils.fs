namespace Backbeat

open Percyqaz.Common
open Percyqaz.Json
open Prelude.Common

module Utils =

    open System.IO
    open System.Runtime.CompilerServices
    open System.Runtime.InteropServices

    type PathHelper() =
        static member Path( [<CallerFilePath; Optional; DefaultParameterValue("")>] path: string ) : string =
            Path.Combine(path, "..", "..") |> Path.GetFullPath

    let REPO_PATH = PathHelper.Path()

    let SETTINGS_FILE = Path.Combine(REPO_PATH, "settings.json")

    [<Json.AutoCodec>]
    type Config = 
        {
            InterludePath: string
        }
        static member Default = { InterludePath = "C:/Interlude/dev" }

    let config : Config = 
        match JSON.FromFile SETTINGS_FILE with
        | Ok c -> c
        | Error e -> Logging.Error("Error loading settings (using default): ", e); Config.Default

    let TABLES_PATH = Path.Combine(REPO_PATH, "tables")
    let CHARTS_PATH = Path.Combine(REPO_PATH, "tables", "charts")
    let PACKS_PATH = Path.Combine(REPO_PATH, "tables", "release")
    let ARCHIVE_PATH = Path.Combine(REPO_PATH, "archive")
    let INTERLUDE_TABLES_PATH = Path.Combine(config.InterludePath, "Data", "Tables")
    let INTERLUDE_COLLECTIONS_FILE = Path.Combine(config.InterludePath, "Data", "collections.json")

    let init() =
        JSON.ToFile (SETTINGS_FILE, true) config
        Directory.CreateDirectory TABLES_PATH |> ignore
        
        Logging.Subscribe
            ( fun (level, main, details) ->
                if level = LoggingLevel.ERROR then printfn "%s" details )
    