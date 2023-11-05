namespace Interlude.Web.Server.Domain

open System
open System.Collections.Generic
open Percyqaz.Common
open Interlude.Web.Shared
open Interlude.Web.Server

module Users =

    let ALPHANUM = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    let SPECIAL = "-_' "
    let VALID_USERNAME_CHARACTERS = ALPHANUM + SPECIAL

    let valid_username (proposed: string) : Result<unit, string> =
        if proposed.Length < 3 then
            Error "Too short"
        elif proposed.Length > 20 then
            Error "Too long"
        elif proposed.Trim().Length <> proposed.Length then
            Error "Trailing/leading whitespace"
        else
            let special_count =
                Seq.where (fun c -> Seq.contains c SPECIAL) proposed |> Seq.length

            if special_count > 2 then
                Error "Max 2 special characters"
            elif proposed.Contains("  ") then
                Error "Contains forbidden spacing"
            elif Seq.forall (fun (c: char) -> Seq.contains c VALID_USERNAME_CHARACTERS) proposed then
                Ok()
            else
                Error "Contains forbidden characters"

    [<RequireQualifiedAccess>]
    type Action =
        | CheckExists of discord_id: uint64 * callback: (bool -> unit)
        | Register of username: string * discord_id: uint64 * callback: (Result<string, string> -> unit)
        | Login of token: string * callback: (Result<int64 * string, string> -> unit)
        | DiscordIdentify of discord_id: uint64 * callback: (Result<string, string> -> unit)

    let private state_change =
        { new Async.Service<Action, unit>() with
            override this.Handle(req) =
                async {
                    match req with

                    | Action.CheckExists(discord_id, callback) -> callback (User.by_discord_id discord_id).IsSome

                    | Action.Register(username, discord_id, callback) ->
                        if (User.by_discord_id discord_id).IsSome then
                            Error "A user is already registered to this Discord account"
                        else
                            match valid_username username with
                            | Error reason -> Error(sprintf "Invalid username (%s)" reason)
                            | Ok() ->
                                if (User.by_username (username)).IsSome then
                                    Error "Username is taken!"
                                else
                                    let user = User.create (username, discord_id)
                                    let id = User.save_new (user)

                                    Logging.Info(
                                        sprintf
                                            "New user '%s' registered with id %i to discord id %i"
                                            username
                                            id
                                            discord_id
                                    )

                                    Ok user.AuthToken
                        |> callback

                    | Action.Login(token, callback) ->
                        match User.by_auth_token token with
                        | Some(id, user) ->
                            User.update_last_seen id
                            Ok(id, user.Username)
                        | None -> Error "Token invalid or expired"
                        |> callback

                    | Action.DiscordIdentify(discord_id, callback) ->
                        match User.by_discord_id discord_id with
                        | Some(id, user) ->
                            let new_token = Guid.NewGuid().ToString("N")
                            User.set_auth_token (id, new_token)
                            Ok new_token
                        | None -> Error "No user is registered to this Discord account"
                        |> callback
                }
        }

    let check_exists (discord_id, callback) =
        state_change.RequestAsync(Action.CheckExists(discord_id, callback))

    let register (username, discord_id, callback) =
        state_change.RequestAsync(Action.Register(username, discord_id, callback))

    let login (token, callback) =
        state_change.RequestAsync(Action.Login(token, callback))

    let discord_identify (discord_id, callback) =
        state_change.RequestAsync(Action.DiscordIdentify(discord_id, callback))

// todo: remember why I made these two different things
// also todo: understand what's wrong with just writing this as locks

[<RequireQualifiedAccess>]
type AuthFlowState =
    | RegisterWaitingCallback of online_session_id: Guid
    | RegisterWaitingUsername of online_session_id: Guid * discord_id: uint64
    | LoginWaitingCallback of online_session_id: Guid

module AuthFlow =

    [<RequireQualifiedAccess>]
    type Action =
        | BeginRegisterWithDiscord of Guid
        | FinishRegisterWithDiscord of Guid * requested_username: string
        | BeginLoginWithDiscord of Guid
        | ReceiveDiscordCallback of flow_id: string * discord_id: uint64 * discord_tag: string

    let private flow_id (session_id: Guid) = session_id.ToString("N")
    let private auth_flows = Dictionary<string, AuthFlowState>()

    let private state_change =
        { new Async.Service<Action, bool>() with
            override this.Handle(req) =
                async {
                    return
                        match req with
                        | Action.BeginRegisterWithDiscord id ->
                            let new_flow = flow_id id
                            auth_flows.[new_flow] <- AuthFlowState.RegisterWaitingCallback id

                            let url =
                                @"https://discord.com/api/oauth2/authorize?client_id=420320424199716864&redirect_uri=https%3A%2F%2F"
                                + SECRETS.ApiBaseUrl
                                + @"%2Fauth%2Fdiscord&response_type=code&scope=identify&state="
                                + new_flow

                            Server.send (id, Downstream.DISCORD_AUTH_URL url)
                            true

                        | Action.BeginLoginWithDiscord id ->
                            let new_flow = flow_id id
                            auth_flows.[new_flow] <- AuthFlowState.LoginWaitingCallback id

                            let url =
                                @"https://discord.com/api/oauth2/authorize?client_id=420320424199716864&redirect_uri=https%3A%2F%2F"
                                + SECRETS.ApiBaseUrl
                                + @"%2Fauth%2Fdiscord&response_type=code&scope=identify&state="
                                + new_flow

                            Server.send (id, Downstream.DISCORD_AUTH_URL url)
                            true

                        | Action.FinishRegisterWithDiscord(id, username) ->
                            let flow_id = flow_id id

                            if not (auth_flows.ContainsKey flow_id) then
                                Server.send (id, Downstream.REGISTRATION_FAILED "This authentication flow has expired")
                                false
                            else

                                match auth_flows.[flow_id] with
                                | AuthFlowState.RegisterWaitingUsername(expected_id, discord_id) ->
                                    if expected_id <> id then
                                        Server.kick (id, "Unexpected registration packet")
                                    else

                                        Users.register (
                                            username,
                                            discord_id,
                                            function
                                            | Ok token ->
                                                auth_flows.Remove(flow_id) |> ignore
                                                Server.send (id, Downstream.AUTH_TOKEN token)
                                            | Error reason -> Server.send (id, Downstream.REGISTRATION_FAILED reason)
                                        )
                                        |> Async.RunSynchronously
                                | _ -> Server.kick (id, "Unexpected registration packet")

                                true

                        | Action.ReceiveDiscordCallback(flow_id, discord_id, discord_tag) ->
                            if not (auth_flows.ContainsKey flow_id) then
                                false
                            else

                                match auth_flows.[flow_id] with
                                | AuthFlowState.RegisterWaitingCallback id ->
                                    let mutable success = true

                                    Users.check_exists (
                                        discord_id,
                                        function
                                        | true ->
                                            Logging.Info(
                                                sprintf
                                                    "Discord account %s(%i) is already registered"
                                                    discord_tag
                                                    discord_id
                                            )

                                            auth_flows.Remove(flow_id) |> ignore

                                            Server.send (
                                                id,
                                                Downstream.REGISTRATION_FAILED(
                                                    sprintf "%s is already linked to an existing account" discord_tag
                                                )
                                            )

                                            success <- false
                                        | false ->
                                            Logging.Info(
                                                sprintf "Ready to link account to %s(%i)" discord_tag discord_id
                                            )

                                            auth_flows.[flow_id] <-
                                                AuthFlowState.RegisterWaitingUsername(id, discord_id)

                                            Server.send (id, Downstream.COMPLETE_REGISTRATION_WITH_DISCORD discord_tag)
                                    )
                                    |> Async.RunSynchronously

                                    success
                                | AuthFlowState.LoginWaitingCallback id ->
                                    let mutable success = true

                                    Users.discord_identify (
                                        discord_id,
                                        function
                                        | Ok token ->
                                            auth_flows.Remove(flow_id) |> ignore
                                            Server.send (id, Downstream.AUTH_TOKEN token)
                                        | Error reason ->
                                            Server.send (id, Downstream.LOGIN_FAILED reason)
                                            success <- false
                                    )
                                    |> Async.RunSynchronously

                                    success
                                | _ -> false
                }
        }

    let begin_register_with_discord id =
        state_change.Request(Action.BeginRegisterWithDiscord id, ignore)

    let begin_login_with_discord id =
        state_change.Request(Action.BeginLoginWithDiscord id, ignore)

    let finish_register_with_discord (id, username) =
        state_change.Request(Action.FinishRegisterWithDiscord(id, username), ignore)

    let receive_discord_callback (flow_id, discord_id, discord_tag) =
        state_change.RequestAsync(Action.ReceiveDiscordCallback(flow_id, discord_id, discord_tag))
