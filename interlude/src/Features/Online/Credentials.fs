namespace Interlude.Features.Online

open System.IO
open Percyqaz.Common
open Percyqaz.Data
open Prelude

[<Json.AutoCodec(false)>]
type Credentials =
    {
        DO_NOT_SHARE_THE_CONTENTS_OF_THIS_FILE_WITH_ANYONE_UNDER_ANY_CIRCUMSTANCES: string
        mutable Username: string
        mutable Token: string
        mutable Host: string
        mutable Api: string
    }
    static member Default =
        {
            DO_NOT_SHARE_THE_CONTENTS_OF_THIS_FILE_WITH_ANYONE_UNDER_ANY_CIRCUMSTANCES =
                "Doing so is equivalent to giving someone your account password"
            Username = ""
            Token = ""
            Host = "online.yavsrg.net"
            Api = "api.yavsrg.net"
        }

    static member Location : string = Path.Combine(get_game_folder "Data", "login.json")

    static member Load() =
        if File.Exists Credentials.Location then
            File.SetAttributes(Credentials.Location, FileAttributes.Normal)

            Credentials.Location
            |> JSON.FromFile
            |> function
                | Ok res -> res
                | Error err ->
                    Logging.Error "Error loading login credentials, you will need to log in again.\n%O" err
                    Credentials.Default
        else
            Credentials.Default

    member this.Save() =
        JSON.ToFile (Credentials.Location, true) this