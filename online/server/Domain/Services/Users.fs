namespace Interlude.Web.Server.Domain.Services

open System
open System.Collections.Generic
open Percyqaz.Common
open Interlude.Web.Shared
open Interlude.Web.Server
open Interlude.Web.Server.Domain.Core

module Users =

    let permanently_delete_user (user_id: int64) =
        Logging.Info "Permanently deleting user with id #%i\nSay goodbye to %A" user_id (User.by_id user_id)

        Friends.on_user_deleted (user_id)

        User.delete user_id
        Logging.Info("Delete successful")

    module Username =

        let private ALPHANUM =
            "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

        let private SPECIAL = "-_' "
        let VALID_CHARACTERS = ALPHANUM + SPECIAL

        let check (proposed_username: string) : Result<unit, string> =
            if proposed_username.Length < 3 then
                Error "Too short"
            elif proposed_username.Length > 20 then
                Error "Too long"
            elif proposed_username.Trim().Length <> proposed_username.Length then
                Error "Trailing/leading whitespace"
            else
                let special_count =
                    Seq.where (fun c -> Seq.contains c SPECIAL) proposed_username |> Seq.length

                if special_count > 2 then
                    Error "Max 2 special characters"
                elif proposed_username.Contains("  ") then
                    Error "Contains forbidden spacing"
                elif Seq.forall (fun (c: char) -> Seq.contains c VALID_CHARACTERS) proposed_username then
                    Ok()
                else
                    Error "Contains forbidden characters"

    module Auth =

        let discord_id_is_taken (discord_id) = (User.by_discord_id discord_id).IsSome

        let REGISTER_LOCK_OBJ = obj ()

        let register (username, discord_id) : Result<string, string> =
            lock REGISTER_LOCK_OBJ
            <| fun () ->
                if (User.by_discord_id discord_id).IsSome then
                    Error "A user is already registered to this Discord account"
                else

                match Username.check username with
                | Error reason -> Error(sprintf "Invalid username (%s)" reason)
                | Ok() ->

                match User.by_username username with
                | Some _ -> Error "Username is taken!"
                | None ->

                let user = User.create (username, discord_id)
                let id = User.save_new user

                Logging.Info "New user '%s' registered with id %i to discord id %i" username id discord_id

                Discord.debug_log (sprintf "🥳 New user '%s' registered! (<@%i>)" username discord_id)

                Ok user.AuthToken

        let login_via_token (token) : Result<int64 * string, unit> =
            match User.by_auth_token token with
            | Some(id, user) ->
                User.update_last_seen id
                Ok(id, user.Username)
            | None -> Error()

        let login_via_discord (discord_id) : Result<string, unit> =
            match User.by_discord_id discord_id with
            | Some(id, _) ->
                let new_token = User.generate_auth_token ()
                User.set_auth_token (id, new_token)
                Ok new_token
            | None -> Error()

        let rename (old_name: string) (new_name: string) =
            match User.by_username old_name with
            | None -> Error (sprintf "User '%s' doesn't exist" old_name)
            | Some (current_user_id, _) ->

            if new_name = old_name then Ok() else

            lock REGISTER_LOCK_OBJ
            <| fun () ->
                match Username.check new_name with
                | Error reason -> Error(sprintf "Invalid new username (%s)" reason)
                | Ok() ->

                match User.by_username new_name with
                | Some (user_id, _) when user_id <> current_user_id -> Error "Username is taken!"
                | _ ->

                User.rename (current_user_id, new_name)
                Ok()

    module DiscordAuthFlow =

        [<RequireQualifiedAccess>]
        type private AuthFlowState =
            | RegisterWaitingUsername of online_session_id: Guid * discord_id: uint64
            | WaitingCallback of online_session_id: Guid

        let private flow_id (session_id: Guid) = session_id.ToString("N")
        let private auth_flows = Dictionary<string, AuthFlowState>()
        let private AUTH_FLOW_LOCK_OBJ = obj ()

        let begin_register_or_login_with_discord id =
            lock AUTH_FLOW_LOCK_OBJ
            <| fun () ->
                let new_flow = flow_id id
                auth_flows.[new_flow] <- AuthFlowState.WaitingCallback id

                let url =
                    @"https://discord.com/api/oauth2/authorize?client_id="
                    + SECRETS.DiscordClientId
                    + "&redirect_uri=https%3A%2F%2F"
                    + SECRETS.ApiBaseUrl
                    + @"%2Fauth%2Fdiscord&response_type=code&scope=identify&state="
                    + new_flow

                Server.send (id, Downstream.DISCORD_AUTH_URL url)

        let finish_register_with_discord (id, username) =
            lock AUTH_FLOW_LOCK_OBJ
            <| fun () ->
                let flow_id = flow_id id

                if not (auth_flows.ContainsKey flow_id) then
                    Server.send (id, Downstream.REGISTRATION_FAILED "This authentication flow has expired")
                    false
                else

                match auth_flows.[flow_id] with
                | AuthFlowState.WaitingCallback _ ->
                    Server.kick (id, "Unexpected registration packet")
                    false
                | AuthFlowState.RegisterWaitingUsername(expected_id, discord_id) ->

                if expected_id <> id then
                    Server.kick (id, "Unexpected registration packet")
                    false
                else

                match Auth.register (username, discord_id) with
                | Error reason ->
                    Server.send (id, Downstream.REGISTRATION_FAILED reason)
                    false
                | Ok token ->

                auth_flows.Remove(flow_id) |> ignore
                Server.send (id, Downstream.AUTH_TOKEN token)
                true

        let receive_discord_callback (flow_id, discord_id, discord_tag) =
            lock AUTH_FLOW_LOCK_OBJ
            <| fun () ->
                if not (auth_flows.ContainsKey flow_id) then
                    false
                else

                match auth_flows.[flow_id] with
                | AuthFlowState.WaitingCallback id ->

                    match Auth.login_via_discord (discord_id) with
                    | Error() ->
                        Logging.Info "Ready to link new account to %s(%i)" discord_tag discord_id
                        auth_flows.[flow_id] <- AuthFlowState.RegisterWaitingUsername(id, discord_id)
                        Server.send (id, Downstream.COMPLETE_REGISTRATION_WITH_DISCORD discord_tag)
                        true
                    | Ok token ->

                    auth_flows.Remove(flow_id) |> ignore
                    Server.send (id, Downstream.AUTH_TOKEN token)
                    true

                | _ -> false