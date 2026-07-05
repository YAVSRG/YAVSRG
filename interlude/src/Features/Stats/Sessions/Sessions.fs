namespace Interlude.Features.Stats

open System
open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Prelude.Data.User.Stats

#nowarn "40"

type SessionsTab() =
    inherit Container(NodeType.Leaf)

    let session_panel = SwapContainer(SessionPanel.CreateCurrent())

    let TODAY = Timestamp.now() |> timestamp_to_rg_calendar_day |> DateOnly.FromDateTime

    let rec selected_session : Setting<(DateOnly * Session) option> =
        Setting.simple None
        |> Setting.trigger (fun v ->
            session_panel.Current <-
                match v with
                | Some (date, session) ->
                    let sessions_today = Stats.STATE.PreviousSessions.[date]
                    SessionPanel.CreatePrevious(session, sessions_today, (fun () -> selected_session.Set None), cycle_session_fd, cycle_session_bk) :> Widget
                | None ->
                    SessionPanel.CreateCurrent()
        )

    and cycle_session_fd() =
        match selected_session.Value with
        | Some (date, session) ->
            let sessions_today = Stats.STATE.PreviousSessions.[date]
            let i = (List.findIndex (fun s -> s = session) sessions_today)
            if i + 1 < sessions_today.Length then
                selected_session.Value <- Some (date, sessions_today.[i + 1])
            else
                let mutable date = date.AddDays(1)
                let mutable found = false
                while date <= TODAY && not found do
                    if Stats.STATE.PreviousSessions.ContainsKey date then
                        found <- true
                        selected_session.Value <- Some (date, Stats.STATE.PreviousSessions.[date].[0])
                    date <- date.AddDays(1)
                if not found then
                    selected_session.Value <- None
        | None ->
            let mutable date = activity.EarliestVisibleDay
            let mutable found = false
            while date <= TODAY && not found do
                if Stats.STATE.PreviousSessions.ContainsKey date then
                    found <- true
                    selected_session.Value <- Some (date, Stats.STATE.PreviousSessions.[date].[0])
                date <- date.AddDays(1)

    and cycle_session_bk() =
        match selected_session.Value with
        | Some (date, session) ->
            let sessions_today = Stats.STATE.PreviousSessions.[date]
            let i = (List.findIndex (fun s -> s = session) sessions_today)
            if i > 0 then
                selected_session.Value <- Some (date, sessions_today.[i - 1])
            else
                let mutable date = date.AddDays(-1)
                let mutable found = false
                let earliest_day = activity.EarliestVisibleDay
                while date >= earliest_day && not found do
                    if Stats.STATE.PreviousSessions.ContainsKey date then
                        found <- true
                        selected_session.Value <- Some (date, List.last Stats.STATE.PreviousSessions.[date])
                    date <- date.AddDays(-1)
                if not found then
                    selected_session.Value <- None
        | None ->
            let mutable date = TODAY
            let mutable found = false
            let earliest_day = activity.EarliestVisibleDay
            while date >= earliest_day && not found do
                if Stats.STATE.PreviousSessions.ContainsKey date then
                    found <- true
                    selected_session.Value <- Some (date,  List.last Stats.STATE.PreviousSessions.[date])
                date <- date.AddDays(-1)

    and activity : RecentActivityGrid = RecentActivityGrid(selected_session)

    override this.Init(parent: Widget) =
        this
            .Add(
                activity
                    .Position(Position.SlicePercentL(0.4f).ShrinkT(200.0f).SliceT(200.0f).ShrinkX(40.0f)),

                SessionTime(
                    (fun () ->
                        match selected_session.Value with
                        | None -> Stats.STATE.CurrentSession.GameTime
                        | Some (_, a) -> a.GameTime
                    ),
                    (fun () ->
                        match selected_session.Value with
                        | None -> Stats.STATE.CurrentSession.PlayTime
                        | Some (_, a) -> a.PlayTime
                    ),
                    (fun () ->
                        match selected_session.Value with
                        | None -> Stats.STATE.CurrentSession.PracticeTime
                        | Some (_, a) -> a.PracticeTime
                    )
                )
                    .Position(Position.SlicePercentL(0.4f).ShrinkT(450.0f).SliceT(250.0f).ShrinkX(40.0f)),

                PlayCount(
                    (fun () ->
                        match selected_session.Value with
                        | None -> Stats.STATE.CurrentSession.PlaysStarted
                        | Some (_, a) -> a.PlaysStarted
                    ),
                    (fun () ->
                        match selected_session.Value with
                        | None -> Stats.STATE.CurrentSession.PlaysCompleted
                        | Some (_, a) -> a.PlaysCompleted
                    ),
                    (fun () ->
                        match selected_session.Value with
                        | None -> Stats.STATE.CurrentSession.PlaysRetried
                        | Some (_, a) -> a.PlaysRetried
                    ),
                    (fun () ->
                        match selected_session.Value with
                        | None -> Stats.STATE.CurrentSession.PlaysQuit
                        | Some (_, a) -> a.PlaysQuit
                    )
                )
                    .Position(Position.SlicePercentL(0.4f).ShrinkT(750.0f).SliceT(250.0f).ShrinkX(40.0f)),

                session_panel
                    .Position(Position.SlicePercentR(0.6f).ShrinkT(40.0f).ShrinkB(80.0f).ShrinkX(40.0f))
            )

        base.Init parent

    override this.Update (elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if this.Focused then
            if (%%"left").Pressed() then
                cycle_session_bk()
            elif (%%"right").Pressed() then
                cycle_session_fd()

    member this.ShowSessionForDate(date: DateOnly) =
        selected_session.Value <- Some (date, Stats.STATE.PreviousSessions.[date].[0])