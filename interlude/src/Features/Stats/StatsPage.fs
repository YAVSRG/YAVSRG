namespace Interlude.Features.Stats

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Interlude.UI
open Interlude.Features.Online

type StatsPage() =
    inherit Page()

    let session_stats = SessionsTab()
    let all_time_stats = OverallTab()
    let leaderboards = LeaderboardsTab()
    let swap = SwapContainer(session_stats)

    let view_date_listener = SkillTimelineGraph.on_view_date.Subscribe(fun date -> session_stats.ShowSessionForDate date; swap.Current <- session_stats)

    override this.Header() =
        let tabs =
            RadioButtons.create_tabs {
                Setting = Setting.make swap.set_Current swap.get_Current
                Options = [|
                    session_stats, %"stats.sessions", K false
                    all_time_stats, %"stats.overall", K false
                    leaderboards, %"stats.leaderboard", fun () -> Network.status <> Network.LoggedIn
                |]
                Height = 50.0f
            }
        tabs.Position <- Position.SlicePercentL(0.4f).ShrinkT(50.0f).SliceT(50.0f).ShrinkX(40.0f)
        tabs

    override this.Content() = swap

    override this.Title = %"menu.stats"
    override this.OnClose() = view_date_listener.Dispose()