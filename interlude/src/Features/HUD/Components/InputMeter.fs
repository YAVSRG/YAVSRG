namespace Interlude.Features.Play.HUD

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Gameplay
open Prelude.Skinning.Noteskins
open Interlude.Features.Play

type InputMeter(user_options: HUDUserOptions, noteskin_options: HUDNoteskinOptions, state: PlayState, should_show_inputs: unit -> bool) =
    inherit StaticWidget(NodeType.None)

    let fades = Array.init state.Chart.Keys (fun _ -> Animation.Fade(0.3f))

    let SCROLL_SPEED = user_options.InputMeterScrollSpeed * 1.0f</ms>

    new(user_options: HUDUserOptions, noteskin_options: HUDNoteskinOptions, state: PlayState) = InputMeter(user_options, noteskin_options, state, K true)

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        for k = 0 to state.Chart.Keys - 1 do
            fades.[k].Update elapsed_ms
            if Bitmask.has_key k state.Scoring.KeyState then
                fades.[k].Value <- 1.0f

    override this.Draw() =
        let box_height = this.Bounds.Width / float32 state.Chart.Keys

        let mutable box = this.Bounds.SliceBottom(box_height).SliceLeft(box_height).Shrink(Style.PADDING)
        for k = 0 to state.Chart.Keys - 1 do
            Draw.rect box (Colors.white.O3a fades.[k].Alpha)
            box <- box.Translate(box_height, 0.0f)

        if should_show_inputs () then
            let recent_events = state.Scoring.ReplayRecentEvents()

            let now = state.CurrentChartTime()
            let point (time: ChartTime) : float32 * Color = 
                let time_ago = now - time
                let offset = time_ago * SCROLL_SPEED
                let height = this.Bounds.Height - box_height
                this.Bounds.Bottom - box_height - offset |> max this.Bounds.Top, Colors.white.O4a (255.0f - offset / height * 255.0f |> int |> min 127 |> max 0)
        
            let mutable previous = now
            let cutoff = now - (this.Bounds.Height - box_height) / SCROLL_SPEED
            for struct (timestamp, keystate) in recent_events |> Seq.takeWhile(fun _ -> previous >= cutoff) do
                for k = 0 to state.Chart.Keys - 1 do
                    if Bitmask.has_key k keystate then
                        let y1, c1 = point timestamp
                        let y2, c2 = point previous
                        Draw.untextured_quad 
                            (Rect.Create(
                                this.Bounds.Left + Style.PADDING + box_height * float32 k,
                                y1,
                                this.Bounds.Left - Style.PADDING + box_height * (1.0f + float32 k),
                                y2
                            ).AsQuad)
                            (Quad.gradient_top_to_bottom c1 c2)
                previous <- timestamp