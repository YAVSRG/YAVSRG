namespace Interlude.Features.Stats

open System
open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Prelude
open Prelude.Data.User.Stats

#nowarn "40"

type SessionsTab() =
    inherit Container(NodeType.Leaf)

    let session_panel =
        SwapContainer(CurrentSession())
            .Position(Position.SlicePercentR(0.6f).ShrinkT(40.0f).ShrinkB(80.0f).ShrinkX(40.0f))

    let TODAY = Timestamp.now() |> timestamp_to_rg_calendar_day |> DateOnly.FromDateTime

    let rec selected_session : Setting<(DateOnly * Session) option> =
        Setting.simple None
        |> Setting.trigger (fun v ->
            session_panel.Current <-
                match v with
                | Some (date, session) ->
                    let sessions_today =
                        PREVIOUS_SESSIONS.[date]
                    PreviousSession(session, sessions_today, (fun () -> selected_session.Set None), cycle_session_fd, cycle_session_bk) :> Widget
                | None ->
                    CurrentSession()
        )

    and cycle_session_fd() =
        match selected_session.Value with
        | Some (date, session) ->
            let sessions_today = PREVIOUS_SESSIONS.[date]
            let i = (List.findIndex (fun s -> s = session) sessions_today)
            if i + 1 < sessions_today.Length then
                selected_session.Value <- Some (date, sessions_today.[i + 1])
            else
                let mutable date = date.AddDays(1)
                let mutable found = false
                while date <= TODAY && not found do
                    if PREVIOUS_SESSIONS.ContainsKey date then
                        found <- true
                        selected_session.Value <- Some (date, PREVIOUS_SESSIONS.[date].[0])
                    date <- date.AddDays(1)
                if not found then
                    selected_session.Value <- None
        | None ->
            let mutable date = activity.EarliestVisibleDay
            let mutable found = false
            while date <= TODAY && not found do
                if PREVIOUS_SESSIONS.ContainsKey date then
                    found <- true
                    selected_session.Value <- Some (date, PREVIOUS_SESSIONS.[date].[0])
                date <- date.AddDays(1)

    and cycle_session_bk() =
        match selected_session.Value with
        | Some (date, session) ->
            let sessions_today = PREVIOUS_SESSIONS.[date]
            let i = (List.findIndex (fun s -> s = session) sessions_today)
            if i > 0 then
                selected_session.Value <- Some (date, sessions_today.[i - 1])
            else
                let mutable date = date.AddDays(-1)
                let mutable found = false
                let earliest_day = activity.EarliestVisibleDay
                while date >= earliest_day && not found do
                    if PREVIOUS_SESSIONS.ContainsKey date then
                        found <- true
                        selected_session.Value <- Some (date, List.last PREVIOUS_SESSIONS.[date])
                    date <- date.AddDays(-1)
                if not found then
                    selected_session.Value <- None
        | None ->
            let mutable date = TODAY
            let mutable found = false
            let earliest_day = activity.EarliestVisibleDay
            while date >= earliest_day && not found do
                if PREVIOUS_SESSIONS.ContainsKey date then
                    found <- true
                    selected_session.Value <- Some (date,  List.last PREVIOUS_SESSIONS.[date])
                date <- date.AddDays(-1)

    and activity : RecentActivityGrid =
        RecentActivityGrid(selected_session)
            .Position(Position.SlicePercentL(0.4f).ShrinkT(200.0f).SliceT(200.0f).ShrinkX(40.0f))

    override this.Init(parent) =
        this
        |+ activity

        |+ SessionTime(
            (fun () ->
                match selected_session.Value with
                | None -> CURRENT_SESSION.GameTime
                | Some (_, a) -> a.GameTime
            ),
            (fun () ->
                match selected_session.Value with
                | None -> CURRENT_SESSION.PlayTime
                | Some (_, a) -> a.PlayTime
            ),
            (fun () ->
                match selected_session.Value with
                | None -> CURRENT_SESSION.PracticeTime
                | Some (_, a) -> a.PracticeTime
            )
        )
            .Position(Position.SlicePercentL(0.4f).ShrinkT(450.0f).SliceT(250.0f).ShrinkX(40.0f))

        |+ PlayCount(
            (fun () ->
                match selected_session.Value with
                | None -> CURRENT_SESSION.PlaysStarted
                | Some (_, a) -> a.PlaysStarted
            ),
            (fun () ->
                match selected_session.Value with
                | None -> CURRENT_SESSION.PlaysCompleted
                | Some (_, a) -> a.PlaysCompleted
            ),
            (fun () ->
                match selected_session.Value with
                | None -> CURRENT_SESSION.PlaysRetried
                | Some (_, a) -> a.PlaysRetried
            ),
            (fun () ->
                match selected_session.Value with
                | None -> CURRENT_SESSION.PlaysQuit
                | Some (_, a) -> a.PlaysQuit
            )
        )
            .Position(Position.SlicePercentL(0.4f).ShrinkT(750.0f).SliceT(250.0f).ShrinkX(40.0f))

        |* session_panel

        base.Init parent

    override this.Update (elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if this.Focused then
            if (%%"left").Pressed() then
                cycle_session_bk()
            elif (%%"right").Pressed() then
                cycle_session_fd()

    member this.ShowSessionForDate(date: DateOnly) =
        selected_session.Value <- Some (date, PREVIOUS_SESSIONS.[date].[0])