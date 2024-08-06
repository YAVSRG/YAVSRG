namespace Interlude.Features.Play.HUD

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Gameplay
open Prelude.Skins.HudLayouts
open Interlude.Features.Play
open Interlude.Features.Gameplay

type InputMeter(config: HudConfig, state: PlayState, should_show_inputs: unit -> bool) =
    inherit StaticWidget(NodeType.None)

    let fades = Array.init state.Chart.Keys (fun _ -> Animation.Delay(float config.InputMeterKeyFadeTime |> max 0.5))

    let SCROLL_SPEED = config.InputMeterScrollSpeed * 1.0f</ms> / SelectedChart.rate.Value

    new(config: HudConfig, state: PlayState) = InputMeter(config, state, K true)

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
                if config.InputMeterScrollDownwards then
                    this.Bounds.SliceT(box_height)
                else
                    this.Bounds.SliceB(box_height)
            )
                .SliceL(box_height)
                .Shrink(Style.PADDING)
        for k = 0 to state.Chart.Keys - 1 do
            let key_alpha = 255.0f - 127.0f * (float32 fades.[k].Time / float32 fades.[k].Interval) |> int
            Draw.rect box (config.InputMeterKeyColor.O3a key_alpha)
            box <- box.Translate(box_height, 0.0f)

        if config.InputMeterShowInputs && should_show_inputs () then
            let recent_events = state.Scoring.ReplayRecentEvents()

            let now = state.CurrentChartTime()
            let point (time: ChartTime) : float32 * Color = 
                let time_ago = now - time
                let height = this.Bounds.Height - box_height
                let offset = time_ago * SCROLL_SPEED |> min height
                
                if config.InputMeterScrollDownwards then 
                    this.Bounds.Top + box_height + offset
                else
                    this.Bounds.Bottom - box_height - offset
                ,
                let mult = 
                    if config.InputMeterInputFadeDistance > 0.0f then
                        (height - offset) / config.InputMeterInputFadeDistance |> min 1.0f
                    else 1.0f
                config.InputMeterInputColor.O4a (float32 config.InputMeterInputColor.A * mult |> int |> min 255 |> max 0)

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
            let fade_edge = now - (this.Bounds.Height - box_height - config.InputMeterInputFadeDistance) / SCROLL_SPEED
            for struct (timestamp, keystate) in recent_events |> Seq.takeWhile(fun _ -> previous >= cutoff) do
                for k = 0 to state.Chart.Keys - 1 do
                    if Bitmask.has_key k keystate then
                        if previous > fade_edge && timestamp < fade_edge then
                            bar (k, fade_edge, previous)
                            bar (k, timestamp, fade_edge)
                        else
                            bar (k, timestamp, previous)
                previous <- timestamp

        if config.InputMeterShowKPS then

            let ONE_SECOND = 1000.0f<ms> * SelectedChart.rate.Value

            let recent_events = state.Scoring.ReplayRecentEvents()
            let now = state.CurrentChartTime()
            let mutable kps = 0.0f
            let mutable previous = 0us
            let mutable previous_time = now
            for struct (timestamp, keystate) in recent_events |> Seq.takeWhile (fun _ -> previous_time >= now - ONE_SECOND) do
                let keys = Bitmask.count (previous &&& ~~~keystate) |> float32
                kps <- kps + keys * (1f - (now - previous_time) / ONE_SECOND)
                previous <- keystate
                previous_time <- timestamp
            let text_bounds = 
                if config.InputMeterScrollDownwards then
                    this.Bounds.ShrinkT(box_height).SliceT(this.Bounds.Width * 0.1f)
                else
                    this.Bounds.ShrinkB(box_height).SliceB(this.Bounds.Width * 0.1f)
            Text.fill_b(Style.font, sprintf "%.0f KPS" kps, text_bounds, Colors.text, Alignment.CENTER)