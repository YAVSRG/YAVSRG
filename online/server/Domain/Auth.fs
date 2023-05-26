namespace Interlude.Web.Server.Domain

open System
open System.Collections.Generic
open Percyqaz.Common
open Interlude.Web.Shared

[<RequireQualifiedAccess>]
type AuthFlowState =
    | RegisterWaitingCallback of online_session_id: Guid
    | RegisterWaitingUsername of online_session_id: Guid * discord_id: int64 * discord_tag: string
    | LoginWaitingCallback of online_session_id: Guid

module AuthFlow = 

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
        | BeginRegisterWithDiscord of Guid
        | FinishRegisterWithDiscord of Guid * flow_id: string * requested_username: string
        | ReceiveDiscordCallback of flow_id: string * discord_id: int64 * discord_tag: string

    let private new_flow_id() = Guid.NewGuid().ToString("N")
    let private auth_flows = Dictionary<string, AuthFlowState>()

    let private state_change = 
        { new Async.Service<Action, Result<unit, string>>() with 
            override this.Handle(req) = 
                async {
                    return
                        match req with
                        | Action.BeginRegisterWithDiscord id ->
                            let new_flow = new_flow_id()
                            auth_flows.Add(new_flow, AuthFlowState.RegisterWaitingCallback id)
                            // send user the callback url via a packet
                            Ok ()

                        | Action.FinishRegisterWithDiscord (id, flow_id, username) ->
                            if not (auth_flows.ContainsKey flow_id) then Error "This authentication flow has expired" else

                            match auth_flows.[flow_id] with
                            | AuthFlowState.RegisterWaitingUsername (expected_id, discord_id, discord_tag) ->
                                if expected_id <> id then Error "Unexpected change in user id" else

                                match valid_username username with
                                | Error reason -> Error (sprintf "Invalid username (%s)" reason)
                                | Ok () ->
                                    // attempt to create user with discord id, username
                                    // create auth token for this user, send to them so they can log in with it
                                    auth_flows.Remove(flow_id) |> ignore
                                    Ok ()
                            | _ -> Error "Received username when not ready"

                        | Action.ReceiveDiscordCallback (flow_id, discord_id, discord_tag) ->
                            if not (auth_flows.ContainsKey flow_id) then Error "This authentication flow has expired" else
                    
                            match auth_flows.[flow_id] with
                            | AuthFlowState.RegisterWaitingCallback id ->
                                auth_flows.[flow_id] <- AuthFlowState.RegisterWaitingUsername(id, discord_id, discord_tag)
                                // send packet to the user to let them know the tag they are linking, and that they can enter a username
                                Ok ()
                            | AuthFlowState.LoginWaitingCallback id ->
                                // check discord id against existing accounts
                                // if exists, refresh auth token for this user, send them token via packet
                                // if not exists, send them a packet noting this
                                Error "not yet implemented"
                            | _ -> Error "Received discord callback when not ready"  
                }
        }

    let begin_register_with_discord id = 
        state_change.RequestAsync(Action.BeginRegisterWithDiscord id)
        
    let finish_register_with_discord (id, flow_id, username) = 
        state_change.RequestAsync(Action.FinishRegisterWithDiscord (id, flow_id, username))

    let receive_discord_callback(flow_id, discord_id, discord_tag) =
        state_change.RequestAsync(Action.ReceiveDiscordCallback (flow_id, discord_id, discord_tag))