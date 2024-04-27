namespace Interlude.Features.Play.HUD

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Skinning.Noteskins
open Interlude.Features
open Interlude.Features.Play

type BPMMeter(user_options: HUDUserOptions, noteskin_options: HUDNoteskinOptions, state: PlayState) as this =
    inherit Container(NodeType.None)

    let first_note = state.WithColors.FirstNote
    let mutable i = 0
    let bpms = state.WithColors.BPM
    let mutable last_seen_time = -Time.infinity

    do
        this
        |* Text(
            (fun () ->
                let ms_per_beat = bpms.[i].Data.MsPerBeat / Gameplay.rate.Value in
                sprintf "%.0f BPM" (60000.0f<ms / minute> / ms_per_beat)
            ),
            Color = K Colors.text_subheading,
            Align = Alignment.CENTER
        )

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        let now = state.CurrentChartTime()

        if now < last_seen_time then
            i <- 0

        last_seen_time <- now

        while i + 1 < bpms.Length && (bpms[i + 1].Time - first_note) < now do
            i <- i + 1
