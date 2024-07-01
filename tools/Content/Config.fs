namespace YAVSRG.CLI.Features.Backbeat

open System.IO
open Percyqaz.Common
open Percyqaz.Data
open Prelude
open Prelude.Data.Library.Caching
open Interlude.Web.Shared
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

    [<Json.AutoCodec>]
    type LoginCredentials = { Api: string; Token: string } with static member Default = { Api = "api.yavsrg.net"; Token = "" }

    do 
        try
            JSON.ToFile (BACKBEAT_SETTINGS_PATH, true) backbeat_config
            let credentials : LoginCredentials = JSON.FromFile(Path.Combine(backbeat_config.InterludePath, "Data", "login.json")) |> expect
            API.Client.init("https://" + credentials.Api)
            API.Client.authenticate(credentials.Token)
        with err -> printfn "%O" err; failwith "Error initialising backbeat utils"

    let interlude_chart_cache = Cache.from_path (Path.Combine(backbeat_config.InterludePath, "Songs"))

    let INTERLUDE_SKINS_PATH = Path.Combine(backbeat_config.InterludePath, "Skins")