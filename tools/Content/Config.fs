namespace YAVSRG.CLI.Features.Backbeat

open System.IO
open Percyqaz.Common
open Percyqaz.Data
open Prelude
open Prelude.Data.Library.Caching
open YAVSRG.CLI

[<AutoOpen>]
module Config =
    
    let BACKBEAT_SETTINGS_PATH = Path.Combine(Utils.YAVSRG_PATH, "backbeat", "settings.json")

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

    let backbeat_config: Config =
        match JSON.FromFile BACKBEAT_SETTINGS_PATH with
        | Ok c -> c
        | Error e ->
            Logging.Error("Error loading settings (using default): ", e)
            Config.Default

    do JSON.ToFile (BACKBEAT_SETTINGS_PATH, true) backbeat_config

    let interlude_chart_cache = Cache.from_path (Path.Combine(backbeat_config.InterludePath, "Songs"))