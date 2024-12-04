namespace Interlude.Features.Stats

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Interlude.UI

type StatsPage() =
    inherit Page()

    let session_stats = SessionsTab()
    let all_time_stats = OverallTab()
    let swap = SwapContainer(session_stats)

    override this.Header() =
        let tabs =
            RadioButtons.create_tabs {
                Setting = Setting.make swap.set_Current swap.get_Current
                Options = [|
                    session_stats, %"stats.sessions", K false
                    all_time_stats, %"stats.overall", K false
                |]
                Height = 50.0f
            }
        tabs.Position <- Position.SliceLPercent(0.4f).ShrinkT(50.0f).SliceT(50.0f).ShrinkX(40.0f)
        tabs

    override this.Content() = swap

    override this.Title = %"menu.stats"
    override this.OnClose() = ()