namespace Interlude.Web.Server.Online

open System
open System.Collections.Generic
open Percyqaz.Common
open Interlude.Web.Shared
open Interlude.Web.Server.Domain.Services

[<RequireQualifiedAccess>]
type SessionState =
    | Nothing
    | Handshake
    | LoggedIn of id: int64 * username: string

module Session =

    let private SESSION_STATE_LOCK_OBJ = Object()
    let private session_id_to_state_map = Dictionary<Guid, SessionState>()
    let private username_to_session_id_map = Dictionary<string, Guid>()

    let connect (session_id: Guid) =
        lock SESSION_STATE_LOCK_OBJ
        <| fun () -> session_id_to_state_map.Add(session_id, SessionState.Nothing)

    let disconnect (session_id: Guid) =
        lock SESSION_STATE_LOCK_OBJ
        <| fun () ->
            if session_id_to_state_map.ContainsKey session_id then
                match session_id_to_state_map.[session_id] with
                | SessionState.LoggedIn(_, username) ->
                    username_to_session_id_map.Remove username |> ignore
                    Logging.Info "[<- %s" username
                | _ -> ()

                session_id_to_state_map.Remove session_id |> ignore

    let handshake (session_id: Guid) =
        lock SESSION_STATE_LOCK_OBJ
        <| fun () ->
            match session_id_to_state_map.[session_id] with
            | SessionState.Nothing ->
                session_id_to_state_map.[session_id] <- SessionState.Handshake
                Server.send (session_id, Downstream.HANDSHAKE_SUCCESS)
            | _ -> Server.kick (session_id, "Handshake sent twice")

    let login (session_id: Guid, auth_token) =
        lock SESSION_STATE_LOCK_OBJ
        <| fun () ->
            match session_id_to_state_map.[session_id] with
            | SessionState.Handshake ->
                match Users.Auth.login_via_token auth_token with
                | Ok(user_id, username) ->
                    if username_to_session_id_map.ContainsKey(username) then
                        let old_session_id = username_to_session_id_map.[username]
                        Server.kick (old_session_id, "Logged in from another location")
                        Server.kick (session_id, "Already logged in somewhere else")
                    else
                        username_to_session_id_map.[username] <- session_id
                        session_id_to_state_map.[session_id] <- SessionState.LoggedIn(user_id, username)
                        Server.send (session_id, Downstream.LOGIN_SUCCESS username)
                        Logging.Info "[-> %s" username
                | Error() ->
                    Logging.Info "%O failed to authenticate" session_id
                    Server.send (session_id, Downstream.LOGIN_FAILED "Login token invalid or expired")
            | SessionState.Nothing -> Server.kick (session_id, "Login sent before handshake")
            | _ -> ()

    let logout (session_id: Guid) =
        lock SESSION_STATE_LOCK_OBJ
        <| fun () ->
            match session_id_to_state_map.[session_id] with
            | SessionState.LoggedIn(_, username) ->
                username_to_session_id_map.Remove username |> ignore
                session_id_to_state_map.[session_id] <- SessionState.Handshake
                Logging.Info "[<- %s" username
            | _ -> Server.kick (session_id, "Not logged in")

    let list_online_users () =
        let states =
            lock SESSION_STATE_LOCK_OBJ
            <| fun () -> Array.ofSeq session_id_to_state_map.Values

        states
        |> Array.choose (
            function
            | SessionState.LoggedIn(id, username) -> Some(id, username)
            | _ -> None
        )

    let find_username_by_session_id (session_id: Guid) =
        let ok, state =
            lock SESSION_STATE_LOCK_OBJ
            <| fun () -> session_id_to_state_map.TryGetValue session_id

        if ok then
            match state with
            | SessionState.LoggedIn(_, username) -> Some username
            | _ -> None
        else
            None

    let find_session_id_by_username (username: string) =
        let ok, session_id =
            lock SESSION_STATE_LOCK_OBJ
            <| fun () -> username_to_session_id_map.TryGetValue username

        if ok then Some session_id else None

    let find_session_ids_by_usernames (usernames: string array) =
        lock SESSION_STATE_LOCK_OBJ
        <| fun () ->
            Array.map
                (fun username ->
                    match username_to_session_id_map.TryGetValue username with
                    | true, id -> Some id
                    | false, _ -> None
                )
                usernames