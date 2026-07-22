namespace Interlude.Features.Stats

open System
open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Prelude.Data.User
open Prelude.Data.User.Stats
open Interlude.Content

#nowarn "40"

type SessionsTab() =
    inherit Container(NodeType.Leaf)

    let session_panel = SwapContainer(SessionPanel.CreateCurrent(Content.Stats.GetCurrentSession()))

    let rec selected_session : Setting<(DateOnly * Session) option> =
        Setting.simple None
        |> Setting.trigger (fun v ->
            session_panel.Current <-
                match v with
                | Some (date, session) ->
                    let sessions_today = Content.Stats.GetPreviousSessionsForDate(date)
                    SessionPanel.CreatePrevious(session, sessions_today, (fun () -> selected_session.Set None), cycle_session_fd, cycle_session_bk) :> Widget
                | None ->
                    SessionPanel.CreateCurrent(Content.Stats.GetCurrentSession())
        )

    and cycle_session_fd() =
        match selected_session.Value with
        | Some (date, session) ->
            selected_session.Value <- Content.Stats.TryGetNextSession(date, session)
        | None ->
            selected_session.Value <- Content.Stats.TryGetEarliestSession()

    and cycle_session_bk() =
        match selected_session.Value with
        | Some (date, session) ->
            selected_session.Value <- Content.Stats.TryGetPreviousSession(date, session)
        | None ->
            selected_session.Value <- Content.Stats.TryGetLatestSession()

    and activity : RecentActivityGrid = RecentActivityGrid(selected_session)

    override this.Init(parent: Widget) =
        this
            .Add(
                activity
                    .Position(Position.SlicePercentL(0.4f).ShrinkT(200.0f).SliceT(200.0f).ShrinkX(40.0f)),

                SessionTime(
                    (fun () ->
                        match selected_session.Value with
                        | None -> Content.Stats.GetCurrentSession().GameTime
                        | Some (_, a) -> a.GameTime
                    ),
                    (fun () ->
                        match selected_session.Value with
                        | None -> Content.Stats.GetCurrentSession().PlayTime
                        | Some (_, a) -> a.PlayTime
                    ),
                    (fun () ->
                        match selected_session.Value with
                        | None -> Content.Stats.GetCurrentSession().PracticeTime
                        | Some (_, a) -> a.PracticeTime
                    )
                )
                    .Position(Position.SlicePercentL(0.4f).ShrinkT(450.0f).SliceT(250.0f).ShrinkX(40.0f)),

                PlayCount(
                    (fun () ->
                        match selected_session.Value with
                        | None -> Content.Stats.GetCurrentSession().PlaysStarted
                        | Some (_, a) -> a.PlaysStarted
                    ),
                    (fun () ->
                        match selected_session.Value with
                        | None -> Content.Stats.GetCurrentSession().PlaysCompleted
                        | Some (_, a) -> a.PlaysCompleted
                    ),
                    (fun () ->
                        match selected_session.Value with
                        | None -> Content.Stats.GetCurrentSession().PlaysRetried
                        | Some (_, a) -> a.PlaysRetried
                    ),
                    (fun () ->
                        match selected_session.Value with
                        | None -> Content.Stats.GetCurrentSession().PlaysQuit
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

    member this.ShowSessionForDate(date: DateOnly) : unit =
        selected_session.Value <- Some (date, Content.Stats.GetPreviousSessionsForDate(date).[0])