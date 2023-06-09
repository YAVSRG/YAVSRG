namespace Interlude.Web.Server.Domain

open System
open Percyqaz.Common

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
            elif proposed.Contains("  ") then Error "Contains forbidden spacing"
            elif Seq.forall (fun (c: char) -> Seq.contains c VALID_USERNAME_CHARACTERS) proposed then Ok()
            else Error "Contains forbidden characters"

    [<RequireQualifiedAccess>]
    type Action =
        | CheckExists of discord_id: uint64 * callback : (bool -> unit)
        | Register of username: string * discord_id: uint64 * callback : (Result<string, string> -> unit)
        | Login of token: string * callback : (Result<string, string> -> unit)
        | DiscordIdentify of discord_id: uint64 * callback : (Result<string, string> -> unit)

    let private state_change = 
        { new Async.Service<Action, unit>() with 
            override this.Handle(req) = 
                async {
                    match req with

                    | Action.CheckExists (discord_id, callback) ->
                        callback (User.by_discord_id discord_id).IsSome

                    | Action.Register (username, discord_id, callback) ->
                        if (User.by_discord_id discord_id).IsSome then Error "A user is already registered to this Discord account"
                        else
                            match valid_username username with
                            | Error reason -> Error (sprintf "Invalid username (%s)" reason)
                            | Ok() -> 
                                if (User.by_username(username)).IsSome then Error "Username is taken!" else
                                let user = User.create(username, discord_id)
                                let id = User.save_new(user)
                                Logging.Info(sprintf "New user '%s' registered with id %i to discord id %i" username id discord_id)
                                Ok user.AuthToken
                        |> callback

                    | Action.Login (token, callback) ->
                        Logging.Info "Received login token"
                        match User.by_auth_token token with
                        | Some (id, user) ->
                            User.save(id, { user with LastLogin = DateTimeOffset.UtcNow.ToUnixTimeMilliseconds() })
                            Ok user.Username
                        | None -> Error "Token invalid or expired"
                        |> callback
                        
                    | Action.DiscordIdentify (discord_id, callback) ->
                        match User.by_discord_id discord_id with
                        | Some (id, user) ->
                            let new_token = Guid.NewGuid().ToString("N")
                            User.save(id, { user with AuthToken = new_token })
                            Ok new_token
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