namespace Interlude.Features.Multiplayer

open System.IO
open Percyqaz.Common
open Percyqaz.Json
open Prelude.Common

[<Json.AutoCodec>]
type Credentials =
    {
        DO_NOT_SHARE_THE_CONTENTS_OF_THIS_FILE_WITH_ANYONE_UNDER_ANY_CIRCUMSTANCES: string
        mutable Username: string
        mutable Host: string
    }
    static member Default =
        {
            DO_NOT_SHARE_THE_CONTENTS_OF_THIS_FILE_WITH_ANYONE_UNDER_ANY_CIRCUMSTANCES = "Doing so is equivalent to giving someone your account password"
            Username = ""
            Host = "online.yavsrg.net"
        }
    static member Location = Path.Combine(getDataPath "Data", "login.json") 
    static member Load() =
        if File.Exists Credentials.Location then
            Credentials.Location
            |> JSON.FromFile
            |> function Ok res -> res | Error e -> Logging.Error("Error loading login credentials, you will need to log in again.", e); Credentials.Default
        else Credentials.Default
    member this.Save() =
        JSON.ToFile (Credentials.Location, true) this
        File.SetAttributes(Credentials.Location, FileAttributes.Hidden)