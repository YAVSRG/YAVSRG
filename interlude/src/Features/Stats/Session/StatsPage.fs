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

    let selected_day = Setting.simple (Timestamp.now() |> timestamp_to_local_day |> DateOnly.FromDateTime)
    let mutable selected_session = Current

    let select_sessions (sessions: Session array) =
        printfn "%A" sessions
        selected_session <- Archived sessions.[0]

    let session_panel = SwapContainer(CurrentSession(), Position = Position.SliceRPercent(0.6f).ShrinkY(40.0f).ShrinkX(40.0f))

    override this.Content() =
        Container(NodeType.Leaf)
        |+ ActivityFeed(selected_day, select_sessions,
            Position = Position.SliceLPercent(0.4f).ShrinkT(200.0f).SliceT(200.0f).ShrinkX(40.0f))

        |+ Playtime(
            (fun () ->
                match selected_session with
                | Current -> Stats.session.GameTime
                | Archived a -> a.GameTime
            ),
            (fun () ->
                match selected_session with
                | Current -> Stats.session.PlayTime
                | Archived a -> a.PlayTime
            ),
            (fun () ->
                match selected_session with
                | Current -> Stats.session.PracticeTime
                | Archived a -> a.PracticeTime
            ),
            Position = Position.SliceLPercent(0.4f).ShrinkT(500.0f).SliceT(200.0f).ShrinkX(40.0f))

        |+ Playcount(
            (fun () ->
                match selected_session with
                | Current -> Stats.session.PlaysStarted
                | Archived a -> a.PlaysStarted
            ),
            (fun () ->
                match selected_session with
                | Current -> Stats.session.PlaysCompleted
                | Archived a -> a.PlaysCompleted
            ),
            (fun () ->
                match selected_session with
                | Current -> Stats.session.PlaysRetried
                | Archived a -> a.PlaysRetried
            ),
            (fun () ->
                match selected_session with
                | Current -> Stats.session.PlaysQuit
                | Archived a -> a.PlaysQuit
            ),
            Position = Position.SliceLPercent(0.4f).ShrinkT(800.0f).SliceT(200.0f).ShrinkX(40.0f))

        |+ session_panel
        :> Widget

    override this.Title = %"menu.stats"
    override this.OnClose() = ()