namespace Interlude.Features.Stats

open System
open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.User

type private SelectedSession =
    | Current
    | Archived of Session

type SessionsTab() =
    inherit Container(NodeType.Leaf)

    let session_panel = SwapContainer(CurrentSession(), Position = Position.SliceRPercent(0.6f).ShrinkY(40.0f).ShrinkX(40.0f))
    let selected_day = Setting.simple (Timestamp.now() |> timestamp_to_local_day |> DateOnly.FromDateTime)
    let mutable selected_session = Current
    let mutable selected_sessions = Map.tryFind selected_day.Value Stats.PREVIOUS_SESSIONS |> Option.defaultValue []

    let rec cycle_session_fd() =
        match selected_session with
        | Archived s ->
            let i = (List.findIndex (fun s2 -> s2 = s) selected_sessions |> (+) 1) % selected_sessions.Length
            selected_session <- Archived selected_sessions.[i]
            session_panel.Current <- PreviousSession(selected_sessions.[i], selected_sessions, show_current, cycle_session_fd, cycle_session_bk)
        | _ -> ()

    and cycle_session_bk() =
        match selected_session with
        | Archived s ->
            let i = (List.findIndex (fun s2 -> s2 = s) selected_sessions |> (+) (selected_sessions.Length - 1)) % selected_sessions.Length
            selected_session <- Archived selected_sessions.[i]
            session_panel.Current <- PreviousSession(selected_sessions.[i], selected_sessions, show_current, cycle_session_fd, cycle_session_bk)
        | _ -> ()

    and show_current() =
        selected_session <- Current
        session_panel.Current <- CurrentSession()

    let select_sessions (sessions: Session list) =
        selected_sessions <- sessions
        match selected_session with
        | Archived s ->
            let i = (List.tryFindIndex (fun s2 -> s2 = s) selected_sessions |> Option.defaultValue -1 |> (+) 1) % selected_sessions.Length
            selected_session <- Archived selected_sessions.[i]
            session_panel.Current <- PreviousSession(selected_sessions.[i], selected_sessions, show_current, cycle_session_fd, cycle_session_bk)
        | _ -> 
        selected_session <- Archived selected_sessions.[0]
        session_panel.Current <- PreviousSession(selected_sessions.[0], selected_sessions, show_current, cycle_session_fd, cycle_session_bk)

    override this.Init(parent) =
        this
        |+ RecentActivityGrid(selected_day, select_sessions,
            Position = Position.SliceLPercent(0.4f).ShrinkT(200.0f).SliceT(200.0f).ShrinkX(40.0f))

        |+ SessionTime(
            (fun () ->
                match selected_session with
                | Current -> Stats.CURRENT_SESSION.GameTime
                | Archived a -> a.GameTime
            ),
            (fun () ->
                match selected_session with
                | Current -> Stats.CURRENT_SESSION.PlayTime
                | Archived a -> a.PlayTime
            ),
            (fun () ->
                match selected_session with
                | Current -> Stats.CURRENT_SESSION.PracticeTime
                | Archived a -> a.PracticeTime
            ),
            Position = Position.SliceLPercent(0.4f).ShrinkT(450.0f).SliceT(250.0f).ShrinkX(40.0f))

        |+ PlayCount(
            (fun () ->
                match selected_session with
                | Current -> Stats.CURRENT_SESSION.PlaysStarted
                | Archived a -> a.PlaysStarted
            ),
            (fun () ->
                match selected_session with
                | Current -> Stats.CURRENT_SESSION.PlaysCompleted
                | Archived a -> a.PlaysCompleted
            ),
            (fun () ->
                match selected_session with
                | Current -> Stats.CURRENT_SESSION.PlaysRetried
                | Archived a -> a.PlaysRetried
            ),
            (fun () ->
                match selected_session with
                | Current -> Stats.CURRENT_SESSION.PlaysQuit
                | Archived a -> a.PlaysQuit
            ),
            Position = Position.SliceLPercent(0.4f).ShrinkT(750.0f).SliceT(250.0f).ShrinkX(40.0f))

        |* session_panel

        base.Init parent