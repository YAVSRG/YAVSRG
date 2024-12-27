namespace Interlude.Features.Play.HUD

open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Skins.HudLayouts
open Interlude.Features.Play
open Interlude.Features.Gameplay

type KeysPerSecondMeter(config: HudConfig, state: PlayState) =
    inherit StaticWidget(NodeType.None)

    let UPDATE_INTERVAL = 200.0
    let mutable update_timer = UPDATE_INTERVAL

    let mutable kps = 0.0f

    let update_kps() =
        let rate = SelectedChart.rate.Value
        let TWO_SECONDS = 2000.0f<ms / rate> * rate

        let recent_events = state.Scoring.EnumerateRecentInputs()
        let now = state.CurrentChartTime()
        kps <- 0.0f
        let mutable previous = 0us
        let mutable previous_time = now
        for struct (timestamp, keystate) in recent_events |> Seq.takeWhile (fun _ -> previous_time >= now - TWO_SECONDS) do
            let keys = Bitmask.count (previous &&& ~~~keystate) |> float32
            kps <- kps + keys * (1f - (now - previous_time) / TWO_SECONDS)
            previous <- keystate
            previous_time <- timestamp

    override this.Draw() =
        let text_bounds = this.Bounds.SlicePercentT 0.5f
        Text.fill_b(Style.font, sprintf "%.0f KPS" kps, text_bounds, Colors.text, Alignment.CENTER)

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        update_timer <- update_timer - elapsed_ms
        if update_timer <= 0.0 then
            update_kps()
            update_timer <- UPDATE_INTERVAL