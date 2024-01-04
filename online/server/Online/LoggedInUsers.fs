namespace Interlude.Web.Server.Online

open System
open System.Collections.Generic
open Percyqaz.Common
open Interlude.Web.Shared
open Interlude.Web.Server.Domain

[<RequireQualifiedAccess>]
type UserState =
    | Nothing
    | Handshake
    | LoggedIn of id: int64 * username: string

module LoggedInUsers =

    [<RequireQualifiedAccess>]
    type Action =
        | Connect of Guid
        | Disconnect of Guid
        | Handshake of Guid
        | Login of Guid * string
        | Logout of Guid

    let private LOCK_OBJ = Object()
    let private user_states = Dictionary<Guid, UserState>()
    let private usernames = Dictionary<string, Guid>()

    // todo: this code CAN be written as locks since it doesn't communicate with any other services just internal state

    let private state_change =
        { new Async.Service<Action, unit>() with
            override this.Handle(req) =
                async {
                    lock LOCK_OBJ
                    <| fun () ->
                        match req with

                        | Action.Connect id -> user_states.Add(id, UserState.Nothing)

                        | Action.Disconnect id ->
                            if user_states.ContainsKey id then
                                match user_states.[id] with
                                | UserState.LoggedIn(_, username) ->
                                    usernames.Remove username |> ignore
                                    Logging.Info(sprintf "[<- %s" username)
                                | _ -> ()

                                user_states.Remove id |> ignore

                        | Action.Handshake id ->
                            match user_states.[id] with
                            | UserState.Nothing ->
                                user_states.[id] <- UserState.Handshake
                                Server.send (id, Downstream.HANDSHAKE_SUCCESS)
                            | _ -> Server.kick (id, "Handshake sent twice")

                        | Action.Login(session_id, token) ->
                            match user_states.[session_id] with
                            | UserState.Handshake ->
                                Users.login (
                                    token,
                                    function
                                    | Ok(user_id, username) ->
                                        if usernames.ContainsKey(username) then
                                            user_states[usernames.[username]] <- UserState.Handshake
                                            Server.kick (usernames.[username], "Logged in from another location")

                                        usernames.[username] <- session_id
                                        user_states.[session_id] <- UserState.LoggedIn(user_id, username)
                                        Server.send (session_id, Downstream.LOGIN_SUCCESS username)
                                        Logging.Info(sprintf "[-> %s" username)
                                    | Error reason ->
                                        Logging.Info(sprintf "%O failed to authenticate: %s" session_id reason)
                                        Server.send (session_id, Downstream.LOGIN_FAILED reason)
                                )
                                |> Async.RunSynchronously
                            | UserState.Nothing -> Server.kick (session_id, "Login sent before handshake")
                            | _ -> Server.kick (session_id, "Login sent twice")

                        | Action.Logout id ->
                            match user_states.[id] with
                            | UserState.LoggedIn(_, username) ->
                                usernames.Remove username |> ignore
                                user_states.[id] <- UserState.Handshake
                                Logging.Info(sprintf "[<- %s" username)
                            | _ -> Server.kick (id, "Not logged in")
                }
        }

    let connect (id) =
        state_change.Request(Action.Connect(id), ignore)

    let disconnect (id) =
        state_change.Request(Action.Disconnect(id), ignore)

    let handshake (id) =
        state_change.Request(Action.Handshake(id), ignore)

    let login (id, username) =
        state_change.Request(Action.Login(id, username), ignore)

    let logout (id) =
        state_change.Request(Action.Logout(id), ignore)

    let who_is_online =
        let service =
            { new Async.Service<unit, (int64 * string) array>() with
                override this.Handle(req) =
                    async {
                        let states = lock LOCK_OBJ <| fun () -> Array.ofSeq user_states.Values

                        return
                            states
                            |> Array.choose (
                                function
                                | UserState.LoggedIn(id, username) -> Some(id, username)
                                | _ -> None
                            )
                    }
            }

        service.RequestAsync

    let find_username =
        let service =
            { new Async.Service<Guid, string option>() with
                override this.Handle(req) =
                    async {
                        let ok, state = lock LOCK_OBJ <| fun () -> user_states.TryGetValue req

                        if ok then
                            match state with
                            | UserState.LoggedIn(_, username) -> return Some username
                            | _ -> return None
                        else
                            return None
                    }
            }

        service.RequestAsync

    let find_sessions =
        let service =
            { new Async.Service<string array, Guid option array>() with
                override this.Handle(req) =
                    async {
                        return
                            lock LOCK_OBJ
                            <| fun () ->
                                Array.map
                                    (fun username ->
                                        match usernames.TryGetValue username with
                                        | true, id -> Some id
                                        | false, _ -> None
                                    )
                                    req
                    }
            }

        service.RequestAsync
