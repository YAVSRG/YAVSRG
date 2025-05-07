namespace Interlude.Features.Play.HUD

open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Gameplay.Replays
open Prelude.Gameplay.Scoring
open Prelude.Skins.HudLayouts
open Interlude.Features.Play
open Interlude.Features.Gameplay
open Interlude.Options

type InputMeter(config: HudConfig, state: PlayState) =
    inherit StaticWidget(NodeType.None)

    let binds = options.GameplayBinds.[state.WithColors.Keys - 3]
    let colors = Array.create state.WithColors.Keys config.InputMeterKeyColor
    let color_fades = Array.init state.WithColors.Keys (fun _ -> Animation.Delay(float config.InputMeterKeyFadeTime * 2.5 |> max 0.5))
    let fades = Array.init state.WithColors.Keys (fun _ -> Animation.Delay(float config.InputMeterKeyFadeTime |> max 0.5))

    let SCROLL_SPEED = config.InputMeterScrollSpeed / SelectedChart.rate.Value

    let lerp_color (percentage: float) (a: byte) (b: byte) =
        (float (b - a) * percentage + float a) |> int |> max 0 |> min 255

    do
        if config.InputMeterJudgementColors then
            state.SubscribeEvents (fun ev ->
                match ev.Action.Judgement with
                | Some (j, _) ->
                    colors.[ev.Column] <- state.Ruleset.JudgementColor j
                    color_fades.[ev.Column].Reset()
                | _ -> ()
            )

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        for k = 0 to state.WithColors.Keys - 1 do
            color_fades.[k].Update elapsed_ms
            fades.[k].Update elapsed_ms
            if Bitmask.has_key k state.Scoring.KeyState then
                fades.[k].Reset()

    override this.Draw() =
        let column_width = this.Bounds.Width / float32 state.WithColors.Keys

        let mutable box =
            (
                if config.InputMeterScrollDownwards then
                    this.Bounds.SliceT(column_width)
                else
                    this.Bounds.SliceB(column_width)
            )
                .SliceL(column_width)
                .ShrinkPercent(config.InputMeterColumnPadding * 0.5f)

        for k = 0 to state.WithColors.Keys - 1 do

            let press_f = float32 fades.[k].Progress
            let key_alpha = float32 config.InputMeterKeyColor.A * (1.0f - 0.5f * press_f) |> int
            let color_f = 1.0 - (1.0 - float press_f) * (1.0 - color_fades.[k].Progress)
            let r = lerp_color color_f colors.[k].R config.InputMeterKeyColor.R
            let g = lerp_color color_f colors.[k].G config.InputMeterKeyColor.G
            let b = lerp_color color_f colors.[k].B config.InputMeterKeyColor.B
            let color = Color.FromArgb(key_alpha, r, g, b)
            Render.rect box color
            if config.InputMeterShowKeybinds then
                Text.fill(Style.font, binds.[k].ToString(), box, config.InputMeterKeybindColor, Alignment.CENTER)
            box <- box.Translate(column_width, 0.0f)

        if config.InputMeterShowInputs then
            let recent_events = state.Scoring.EnumerateRecentFrames()

            let now = state.CurrentChartTime()
            let point (time: ChartTime) : float32 * Color =
                let time_ago = now - time
                let height = this.Bounds.Height - column_width
                let offset = time_ago * SCROLL_SPEED |> min height

                if config.InputMeterScrollDownwards then
                    this.Bounds.Top + column_width + offset
                else
                    this.Bounds.Bottom - column_width - offset
                ,
                let mult =
                    if config.InputMeterInputFadeDistance > 0.0f then
                        (height - offset) / config.InputMeterInputFadeDistance |> min 1.0f
                    else 1.0f
                config.InputMeterInputColor.O4a (float32 config.InputMeterInputColor.A * mult |> int |> min 255 |> max 0)

            let inline bar (k, timestamp, previous) =
                let y1, c1 = point timestamp
                let y2, c2 = point previous
                Render.rect_c
                    (Rect.FromEdges(
                        this.Bounds.Left + column_width * float32 k,
                        y1,
                        this.Bounds.Left + column_width * (1.0f + float32 k),
                        y2
                    ).SliceX(box.Width))
                    (Quad.gradient_top_to_bottom c1 c2)

            let mutable previous = now
            let cutoff = now - (this.Bounds.Height - column_width) / SCROLL_SPEED
            let fade_edge = now - (this.Bounds.Height - column_width - config.InputMeterInputFadeDistance) / SCROLL_SPEED
            for struct (timestamp, keystate) in recent_events |> Seq.takeWhile(fun _ -> previous >= cutoff) do
                for k = 0 to state.WithColors.Keys - 1 do
                    if Bitmask.has_key k keystate then
                        if previous > fade_edge && timestamp < fade_edge then
                            bar (k, fade_edge, previous)
                            bar (k, timestamp, fade_edge)
                        else
                            bar (k, timestamp, previous)
                previous <- timestamp