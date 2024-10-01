namespace Interlude.Features.Play.HUD

open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Skins.HudLayouts
open Interlude.Features.Play
open Interlude.Features.Gameplay

type KeysPerSecondMeter(config: HudConfig, state: PlayState) =
    inherit StaticWidget(NodeType.None)

    override this.Draw() =
        let rate = SelectedChart.rate.Value
        let TWO_SECONDS = 2000.0f<ms / rate> * rate

        let recent_events = state.Scoring.EnumerateRecentInputs()
        let now = state.CurrentChartTime()
        let mutable kps = 0.0f
        let mutable previous = 0us
        let mutable previous_time = now
        for struct (timestamp, keystate) in recent_events |> Seq.takeWhile (fun _ -> previous_time >= now - TWO_SECONDS) do
            let keys = Bitmask.count (previous &&& ~~~keystate) |> float32
            kps <- kps + keys * (1f - (now - previous_time) / TWO_SECONDS)
            previous <- keystate
            previous_time <- timestamp
        let text_bounds = this.Bounds.SlicePercentT 0.5f
        Text.fill_b(Style.font, sprintf "%.0f KPS" kps, text_bounds, Colors.text, Alignment.CENTER)