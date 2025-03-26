namespace Interlude.Features.Play.HUD

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Skins.HudLayouts
open Interlude.Features.Gameplay
open Interlude.Features.Play

type BPM(config: HudConfig, state: PlayState) =
    inherit Container(NodeType.None)

    let first_note = state.WithColors.FirstNote
    let mutable i = 0
    let bpms = state.WithColors.BPM
    let mutable last_seen_time = -Time.infinity

    override this.Init(parent) =
        this
        |* Text(
            (fun () ->
                let ms_per_beat = bpms.[i].Data.MsPerBeat / SelectedChart.rate.Value in
                sprintf "%.0f BPM" (60000.0f<ms / minute> / ms_per_beat)
            ))
            .Color(Colors.text_subheading)
            .Align(Alignment.CENTER)
        base.Init parent

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        let now = state.CurrentChartTime()

        if now < last_seen_time then
            i <- 0

        last_seen_time <- now

        while i + 1 < bpms.Length && (bpms[i + 1].Time - first_note) < now do
            i <- i + 1