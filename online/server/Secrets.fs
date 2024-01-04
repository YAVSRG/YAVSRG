﻿namespace Interlude.Web.Server

open Percyqaz.Json
open System.IO
open System.Reflection

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
            failwith
                "Secrets folder not found! You are running the server as a non-docker instance, make sure you ran it with the script"
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
