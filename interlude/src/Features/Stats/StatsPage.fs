namespace Interlude.Features.Stats

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Interlude.UI
open Interlude.Features.Online

type StatsPage() =
    inherit Page()

    static let selected_tab = Setting.simple 0
    let tab_container = SwapContainer()

    let session_stats = SessionsTab()
    let all_time_stats = OverallTab.Create()
    let leaderboards = LeaderboardsTab.Create()

    let view_date_listener = SkillTimelineGraph.on_view_date.Subscribe(fun date -> session_stats.ShowSessionForDate date; tab_container.Current <- session_stats)

    let tab_options : (Widget * string * (unit -> bool)) array =
        [|
            session_stats, %"stats.sessions", K false
            all_time_stats, %"stats.overall", K false
            leaderboards, %"stats.leaderboard", fun () -> Network.status <> Network.LoggedIn
        |]

    override this.Header() =
        if selected_tab.Value = 2 && Network.status <> Network.LoggedIn then selected_tab.Value <- 0

        TabButtons.CreatePersistent(tab_options, tab_container, selected_tab)
            .Position(Position.SlicePercentL(0.4f).ShrinkT(50.0f).SliceT(TabButtons.HEIGHT).ShrinkX(40.0f))

    override this.Content() = tab_container

    override this.Title = %"menu.stats"
    override this.OnClose() = view_date_listener.Dispose()