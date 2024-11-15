namespace Interlude.Features.Stats

open System
open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.User
open Interlude.UI

type private SelectedSession =
    | Current
    | Archived of Session

type StatsPage() =
    inherit Page()

    let session_panel = SwapContainer(CurrentSession(), Position = Position.SliceRPercent(0.6f).ShrinkY(40.0f).ShrinkX(40.0f))
    let selected_day = Setting.simple (Timestamp.now() |> timestamp_to_local_day |> DateOnly.FromDateTime)
    let mutable selected_session = Current

    let select_sessions (sessions: Session list) =
        match selected_session with
        | Archived s ->
            let i = (List.tryFindIndex (fun s2 -> s2 = s) sessions |> Option.defaultValue -1 |> (+) 1) % sessions.Length
            selected_session <- Archived sessions.[i]
            session_panel.Current <- PreviousSession(sessions.[i])
        | _ -> 
            selected_session <- Archived sessions.[0]
            session_panel.Current <- PreviousSession(sessions.[0])

    override this.Content() =
        Container(NodeType.Leaf)
        |+ ActivityFeed(selected_day, select_sessions,
            Position = Position.SliceLPercent(0.4f).ShrinkT(200.0f).SliceT(200.0f).ShrinkX(40.0f))

        |+ Playtime(
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

        |+ Playcount(
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

        |+ session_panel
        :> Widget

    override this.Title = %"menu.stats"
    override this.OnClose() = ()