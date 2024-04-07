namespace Interlude.Features.Play.HUD

open System
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Content.Noteskins
open Interlude.Features
open Interlude.Features.Play

type ProgressMeter(user_options: HUDUserOptions, noteskin_options: HUDNoteskinOptions, state: PlayState) =
    inherit StaticWidget(NodeType.None)

    let duration =
        let chart = state.WithColors
        chart.LastNote - chart.FirstNote

    override this.Draw() =
        let now = state.CurrentChartTime()
        let pc = now / duration |> max 0.0f |> min 1.0f

        let x, y = this.Bounds.Center
        let r = (min this.Bounds.Width this.Bounds.Height) * 0.5f
        let angle = MathF.PI / 15.0f

        let outer i =
            let angle = float32 i * angle
            let struct (a, b) = MathF.SinCos(angle)
            (x + r * a, y - r * b)

        let inner i =
            let angle = float32 i * angle
            let struct (a, b) = MathF.SinCos(angle)
            (x + (r - 4f) * a, y - (r - 4f) * b)

        for i = 0 to 29 do
            Draw.untextured_quad
                (Quad.createv (x, y) (x, y) (inner i) (inner (i + 1)))
                (Quad.color noteskin_options.ProgressMeterBackgroundColor)

            Draw.untextured_quad
                (Quad.createv (inner i) (outer i) (outer (i + 1)) (inner (i + 1)))
                (Quad.color Colors.white.O2)

        for i = 0 to pc * 29.9f |> floor |> int do
            Draw.untextured_quad
                (Quad.createv (x, y) (x, y) (inner i) (inner (i + 1)))
                (Quad.color noteskin_options.ProgressMeterColor)

        let text =
            match user_options.ProgressMeterLabel with
            | ProgressMeterLabel.Countdown ->
                let time_left = (duration - now) / Gameplay.rate.Value |> max 0.0f<ms>

                sprintf
                    "%i:%02i"
                    (time_left / 60000.0f<ms> |> floor |> int)
                    ((time_left % 60000.0f<ms>) / 1000.0f<ms> |> floor |> int)
            | ProgressMeterLabel.Percentage -> sprintf "%.0f%%" (pc * 100.0f)
            | _ -> ""

        Text.fill_b (
            Style.font,
            text,
            this.Bounds.BorderBottom(40.0f),
            Colors.text_subheading,
            Alignment.CENTER
        )