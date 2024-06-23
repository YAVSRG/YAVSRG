namespace Interlude.Features.Play.HUD

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Gameplay
open Prelude.Skinning.Noteskins
open Interlude.Features.Play
open Interlude.Features.Gameplay

type InputMeter(user_options: HUDUserOptions, noteskin_options: HUDNoteskinOptions, state: PlayState, should_show_inputs: unit -> bool) =
    inherit StaticWidget(NodeType.None)

    let fades = Array.init state.Chart.Keys (fun _ -> Animation.Delay(float noteskin_options.InputMeterKeyFadeTime |> max 0.5))

    let SCROLL_SPEED = user_options.InputMeterScrollSpeed * 1.0f</ms> / SelectedChart.rate.Value

    new(user_options: HUDUserOptions, noteskin_options: HUDNoteskinOptions, state: PlayState) = InputMeter(user_options, noteskin_options, state, K true)

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        for k = 0 to state.Chart.Keys - 1 do
            fades.[k].Update elapsed_ms
            if Bitmask.has_key k state.Scoring.KeyState then
                fades.[k].Reset()

    override this.Draw() =
        let box_height = this.Bounds.Width / float32 state.Chart.Keys

        let mutable box = 
            (
                if noteskin_options.InputMeterScrollDownwards then
                    this.Bounds.SliceTop(box_height)
                else
                    this.Bounds.SliceBottom(box_height)
            )
                .SliceLeft(box_height)
                .Shrink(Style.PADDING)
        for k = 0 to state.Chart.Keys - 1 do
            let key_alpha = 255.0f - 127.0f * (float32 fades.[k].Time / float32 fades.[k].Interval) |> int
            Draw.rect box (noteskin_options.InputMeterKeyColor.O3a key_alpha)
            box <- box.Translate(box_height, 0.0f)

        if noteskin_options.InputMeterShowInputs && should_show_inputs () then
            let recent_events = state.Scoring.ReplayRecentEvents()

            let now = state.CurrentChartTime()
            let point (time: ChartTime) : float32 * Color = 
                let time_ago = now - time
                let offset = time_ago * SCROLL_SPEED
                let height = this.Bounds.Height - box_height
                
                if noteskin_options.InputMeterScrollDownwards then 
                    this.Bounds.Top + box_height + offset |> min this.Bounds.Bottom
                else
                    this.Bounds.Bottom - box_height - offset |> max this.Bounds.Top
                ,
                let mult = (height - offset) / noteskin_options.InputMeterInputFadeDistance |> min 1.0f
                noteskin_options.InputMeterInputColor.O4a (255.0f * mult |> int |> min (int noteskin_options.InputMeterInputColor.A) |> max 0)

            let inline bar (k, timestamp, previous) =
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
        
            let mutable previous = now
            let cutoff = now - (this.Bounds.Height - box_height) / SCROLL_SPEED
            let fade_edge = now - (this.Bounds.Height - box_height - noteskin_options.InputMeterInputFadeDistance) / SCROLL_SPEED
            for struct (timestamp, keystate) in recent_events |> Seq.takeWhile(fun _ -> previous >= cutoff) do
                for k = 0 to state.Chart.Keys - 1 do
                    if Bitmask.has_key k keystate then
                        if previous > fade_edge && timestamp < fade_edge then
                            bar (k, fade_edge, previous)
                            bar (k, timestamp, fade_edge)
                        else
                            bar (k, timestamp, previous)
                previous <- timestamp