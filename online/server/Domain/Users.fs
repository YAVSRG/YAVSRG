namespace Interlude.Web.Server.Domain

open System
open System.Linq
open System.Collections.Generic
open Percyqaz.Common

type User =
    {
        Id: int
        Username: string
        DiscordId: int64
        mutable LastLogin: DateTime
        mutable AuthToken: string
    }

module Users = 

    let ALPHANUM = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    let SPECIAL = "-_' "
    let VALID_USERNAME_CHARACTERS = ALPHANUM + SPECIAL
    let valid_username (proposed: string) : Result<unit, string> =
        if proposed.Length < 3 then Error "Too short"
        elif proposed.Length > 20 then Error "Too long"
        elif proposed.Trim().Length <> proposed.Length then Error "Trailing/leading whitespace"
        else
            let special_count = Seq.where (fun c -> Seq.contains c SPECIAL) proposed |> Seq.length
            if special_count > 2 then Error "Max 2 special characters"
            elif Seq.forall (fun (c: char) -> Seq.contains c VALID_USERNAME_CHARACTERS) proposed then Ok()
            else Error "Contains forbidden characters"

    [<RequireQualifiedAccess>]
    type Action =
        | CheckExists of discord_id: int64 * callback : (bool -> unit)
        | Register of username: string * discord_id: int64 * callback : (Result<string, string> -> unit)
        | Login of token: string * callback : (Result<string, string> -> unit)
        | DiscordIdentify of discord_id: int64 * callback : (Result<string, string> -> unit)

    let private users = List<User>()

    let private state_change = 
        { new Async.Service<Action, unit>() with 
            override this.Handle(req) = 
                async {
                    match req with

                    | Action.CheckExists (discord_id, callback) ->
                        callback (users.Any(fun u -> u.DiscordId = discord_id))

                    | Action.Register (username, discord_id, callback) ->
                        if users.Any(fun u -> u.DiscordId = discord_id) then Error "A user is already registered to this Discord account"
                        else
                            match valid_username username with
                            | Error reason -> Error (sprintf "Invalid username (%s)" reason)
                            | Ok() -> 
                                if users.Any(fun u -> u.Username.ToLowerInvariant() = username.ToLowerInvariant()) then Error "Username is taken!" else
                                let user = 
                                    { 
                                        Id = users.Count + 1
                                        Username = username
                                        DiscordId = discord_id
                                        LastLogin = DateTime.Now
                                        AuthToken = Guid.NewGuid().ToString("N")
                                    }
                                users.Add(user)
                                Ok user.AuthToken
                        |> callback

                    | Action.Login (token, callback) ->
                        Logging.Info "Received login token"
                        match users |> Seq.tryFind (fun u -> u.AuthToken = token) with
                        | Some user -> Ok user.Username
                        | None -> Error "Token invalid or expired"
                        |> callback
                        
                    | Action.DiscordIdentify (discord_id, callback) ->
                        match users |> Seq.tryFind (fun u -> u.DiscordId = discord_id) with
                        | Some user ->
                            user.AuthToken <- Guid.NewGuid().ToString("N")
                            Ok user.AuthToken
                        | None -> Error "No user is registered to this Discord account"
                        |> callback
                }
        }

    let check_exists (discord_id, callback) =
        state_change.RequestAsync (Action.CheckExists (discord_id, callback))

    let register (username, discord_id, callback) =
        state_change.RequestAsync (Action.Register (username, discord_id, callback))

    let login (token, callback) =
        state_change.RequestAsync (Action.Login (token, callback))

    let discord_identify (discord_id, callback) =
        state_change.RequestAsync (Action.DiscordIdentify (discord_id, callback))