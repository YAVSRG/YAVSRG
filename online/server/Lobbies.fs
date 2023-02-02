namespace Interlude.Web.Server

open System
open System.Collections.Generic
open Percyqaz.Common
open Interlude.Web.Shared

//type Lobby =
//    {
//        Owner: Guid
//        mutable Settings: LobbySettings
//        mutable Host: Guid
//        mutable Chart: LobbyChart option
//    }

//module Lobby = 

//    [<RequireQualifiedAccess>]
//    type Action =
//        | Connect of Guid
//        | Disconnect of Guid
//        | Handshake of Guid
//        | Login of Guid * string

//    let private lobbies = Dictionary<Guid, Lobby>()

//    let valid_lobby_name (proposed: string) : bool =
//        if (proposed.Length < 2 || proposed.Length > 30) then false else

//        if proposed.Trim().Length <> proposed.Length then false else

//        (Seq.forall (fun (c: char) -> Seq.contains c UserState.VALID_USERNAME_CHARACTERS) proposed)

//    let private state_change = 
//        { new Async.Service<Action, unit>()
//            with override this.Handle(req) = async {
//                    match req with

//                    | Action.Connect id ->
//                        lobbies.Add(id, UserState.Nothing)
//            }
//        }