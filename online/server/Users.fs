namespace Interlude.Web.Server

open System
open System.Collections.Concurrent
open Percyqaz.Common
open Interlude.Web.Shared

[<RequireQualifiedAccess>]
type UserState =
    | Nothing
    | Handshake
    | LoggedIn of username: string

module UserState = 

    [<RequireQualifiedAccess>]
    type Action =
        | Connect of Guid
        | Disconnect of Guid
        | Handshake of Guid
        | Login of Guid * string

    let private user_states = ConcurrentDictionary<Guid, UserState>()
    let private usernames = ConcurrentDictionary<string, Guid>()

    let valid_username (proposed: string) : bool =
        if (proposed.Length < 2 || proposed.Length > 20) then false else

        if proposed.Trim().Length <> proposed.Length then false else

        (Seq.forall (fun (c: char) -> Char.IsBetween(c, ' ', '~')) proposed)

    let private state_change = 
        { new Async.Service<Action, bool>()
            with override this.Handle(req) = async {
                    match req with
                    | Action.Connect id -> 
                        return user_states.TryAdd(id, UserState.Nothing)
                    | Action.Disconnect id -> 
                        let ok, state = user_states.TryRemove(id)
                        if ok then
                            match state with
                            | UserState.LoggedIn username ->
                                assert((true, id) = usernames.TryRemove username)
                                Logging.Info(sprintf "<- %s" username)
                            | _ -> ()
                        return ok
                    | Action.Handshake id ->
                        return user_states.TryUpdate(id, UserState.Handshake, UserState.Nothing)
                    | Action.Login (id, username) ->
                        if valid_username username then
                            if usernames.TryAdd(username, id) then
                                Logging.Info(sprintf "-> %s" username)
                                Server.send(id, Downstream.LOGIN_SUCCESS username)
                                return user_states.TryUpdate(id, UserState.LoggedIn username, UserState.Handshake)
                            else
                                Server.kick(id, "Username is taken")
                                return false
                        else
                            Server.kick(id, "Invalid username")
                            return false
            }
        }

    let connect(id) =
        state_change.Request(Action.Connect (id), ignore)

    let disconnect(id) =
        state_change.Request(Action.Disconnect (id), ignore)

    let handshake(id) =
        state_change.Request(Action.Handshake (id), ignore)

    let login(id, username) =
        state_change.Request(Action.Login (id, username), ignore)

    let private username_lookup =
        { new Async.Service<Guid, string option>()
            with override this.Handle(req) = async {
                    let ok, state = user_states.TryGetValue req
                    if ok then
                        match state with
                        | UserState.LoggedIn username -> return Some username
                        | _ -> return None
                    else return None
            }
        }

    let find_username(id: Guid) = username_lookup.RequestAsync id

    let private session_lookup =
        { new Async.Service<string, Guid option>()
            with override this.Handle(req) = async {
                    let ok, id = usernames.TryGetValue req
                    if ok then return Some id
                    else return None
            }
        }

    let find_session(username: string) = session_lookup.RequestAsync username