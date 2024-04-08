namespace Interlude.Web.Server

open System.IO
open System.Reflection
open Percyqaz.Common
open Percyqaz.Data

[<Json.AutoCodec(false)>]
type Secrets =
    {
        SocketCert: string
        SocketCertPassword: string
        ApiCert: string
        ApiCertPassword: string
        ApiBaseUrl: string
        DiscordBotToken: string
        DiscordClientId: string
        DiscordClientSecret: string
        IsProduction: bool
    }
    static member Default =
        {
            SocketCert = "localhost.pfx"
            SocketCertPassword = "DEVELOPMENT"
            ApiCert = "localhost.pfx"
            ApiCertPassword = "DEVELOPMENT"
            ApiBaseUrl = "localhost"
            DiscordBotToken = ""
            DiscordClientId = ""
            DiscordClientSecret = ""
            IsProduction = false
        }

[<AutoOpen>]
module Secrets =

    let SECRETS =
        if not (File.Exists "./secrets/secrets.json") then
#if DEBUG
            Logging.Info
                "!!! Server assembly is being run outside of docker, or examined in unit tests. Using default values for secrets"

            Secrets.Default
        else
#else
            failwith "Secrets folder not found! Did you mount it properly?"
#endif
        match Prelude.Common.JSON.FromFile<Secrets>("./secrets/secrets.json") with
        | Ok o -> o
        | Error e -> failwithf "Error while reading secrets.json: %O" e

    let TAGLINE =
        let stream =
            Assembly
                .GetExecutingAssembly()
                .GetManifestResourceStream("Interlude.Web.Server.Version.txt")

        use tr = new StreamReader(stream)
        tr.ReadToEnd()

open Percyqaz.Data.Sqlite

[<AutoOpen>]
module internal DatabaseRef =

    let mutable core_db = Unchecked.defaultof<Database>
    let mutable backbeat_db = Unchecked.defaultof<Database>

module Discord =

    let mutable debug_log = fun (s: string) -> ()
    let mutable feed_log = fun (s: string) -> ()
