namespace Interlude.Web.Server.Domain

open System
open System.Collections.Generic
open Percyqaz.Common
open Interlude.Web.Shared
open Interlude.Web.Server

[<RequireQualifiedAccess>]
type AuthFlowState =
    | RegisterWaitingCallback of online_session_id: Guid
    | RegisterWaitingUsername of online_session_id: Guid * discord_id: int64
    | LoginWaitingCallback of online_session_id: Guid

module AuthFlow = 

    [<RequireQualifiedAccess>]
    type Action =
        | BeginRegisterWithDiscord of Guid
        | FinishRegisterWithDiscord of Guid * requested_username: string
        | BeginLoginWithDiscord of Guid
        | ReceiveDiscordCallback of flow_id: string * discord_id: int64 * discord_tag: string

    let private flow_id(session_id: Guid) = session_id.ToString("N")
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
                            Server.send(id, Downstream.DISCORD_AUTH_URL url)
                            true

                        | Action.BeginLoginWithDiscord id ->
                            let new_flow = flow_id id
                            auth_flows.[new_flow] <- AuthFlowState.LoginWaitingCallback id
                            let url = 
                                @"https://discord.com/api/oauth2/authorize?client_id=420320424199716864&redirect_uri=https%3A%2F%2F" 
                                + SECRETS.ApiBaseUrl
                                + @"%2Fauth%2Fdiscord&response_type=code&scope=identify&state="
                                + new_flow
                            Server.send(id, Downstream.DISCORD_AUTH_URL url)
                            true

                        | Action.FinishRegisterWithDiscord (id, username) ->
                            let flow_id = flow_id id
                            if not (auth_flows.ContainsKey flow_id) then Server.send(id, Downstream.REGISTRATION_FAILED "This authentication flow has expired"); false else

                            match auth_flows.[flow_id] with
                            | AuthFlowState.RegisterWaitingUsername (expected_id, discord_id) ->
                                if expected_id <> id then Server.kick(id, "Unexpected registration packet") else

                                Users.register (username, discord_id,
                                    function
                                    | Ok token ->
                                        auth_flows.Remove(flow_id) |> ignore
                                        Server.send(id, Downstream.AUTH_TOKEN token)
                                    | Error reason -> Server.send(id, Downstream.REGISTRATION_FAILED reason)
                                    ) |> Async.RunSynchronously
                            | _ -> Server.kick(id, "Unexpected registration packet")
                            true

                        | Action.ReceiveDiscordCallback (flow_id, discord_id, discord_tag) ->
                            if not (auth_flows.ContainsKey flow_id) then false else
                    
                            match auth_flows.[flow_id] with
                            | AuthFlowState.RegisterWaitingCallback id ->
                                auth_flows.[flow_id] <- AuthFlowState.RegisterWaitingUsername(id, discord_id)
                                Server.send(id, Downstream.COMPLETE_REGISTRATION_WITH_DISCORD discord_tag)
                                true
                            | AuthFlowState.LoginWaitingCallback id ->
                                Users.discord_identify (discord_id,
                                    function
                                    | Ok token ->
                                        auth_flows.Remove(flow_id) |> ignore
                                        Server.send(id, Downstream.AUTH_TOKEN token)
                                    | Error reason -> Server.send(id, Downstream.LOGIN_FAILED reason)
                                    ) |> Async.RunSynchronously
                                true
                            | _ -> false
                }
        }

    let begin_register_with_discord id = 
        state_change.Request(Action.BeginRegisterWithDiscord id, ignore)
        
    let begin_login_with_discord id = 
        state_change.Request(Action.BeginLoginWithDiscord id, ignore)
        
    let finish_register_with_discord (id, username) = 
        state_change.Request(Action.FinishRegisterWithDiscord (id, username), ignore)

    let receive_discord_callback(flow_id, discord_id, discord_tag) =
        state_change.RequestAsync(Action.ReceiveDiscordCallback (flow_id, discord_id, discord_tag))