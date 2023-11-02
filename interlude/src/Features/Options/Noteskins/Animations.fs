namespace Interlude.Features.OptionsMenu.Noteskins

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.Data.Content
open Interlude.Content
open Interlude.Utils
open Interlude.UI.Menu

type AnimationSettingsPage() as this =
    inherit Page()

    let data = Noteskins.Current.config

    let enable_column_light = Setting.simple data.EnableColumnLight

    let column_light_time =
        Setting.bounded data.ColumnLightTime 0.1f 1.0f |> Setting.roundf 2

    let note_animation_time = Setting.bounded data.AnimationFrameTime 10.0 1000.0

    // todo: replace column and explosion fade percentages with a real timing system
    // todo: option to disable fading of explosions
    let enable_explosions = Setting.simple data.Explosions.Enable

    let explosion_animation_time =
        Setting.bounded data.Explosions.AnimationFrameTime 10.0 1000.0

    let explosion_fade_time =
        Setting.bounded data.Explosions.FadeTime 0.1f 1.0f |> Setting.roundf 2

    let explosion_scale = Setting.bounded data.Explosions.Scale 0.5f 2.0f
    let explosion_expand = Setting.percentf data.Explosions.ExpandAmount
    let explosion_on_miss = Setting.simple data.Explosions.ExplodeOnMiss
    let explosion_colors = Setting.simple data.Explosions.Colors

    let note = get_texture "note"
    let noteexplosion = get_texture "noteexplosion"
    let holdexplosion = get_texture "holdexplosion"
    let receptor = get_texture "receptor"
    let columnlighting = get_texture "receptorlighting"
    let mutable test_event_i = 0

    let test_events = Animation.Counter 1000.0
    let note_frames = Animation.Counter(data.AnimationFrameTime)
    let explosion_frames = Animation.Counter(data.Explosions.AnimationFrameTime)
    let explosion_fade = Animation.Fade 0.0f
    let hold_explosion_fade = Animation.Fade 0.0f

    do
        this.Content(
            column ()
            |+ PageSetting("noteskins.edit.enablecolumnlight", Selector<_>.FromBool enable_column_light)
                .Pos(200.0f)
                .Tooltip(Tooltip.Info("noteskins.edit.enablecolumnlight"))
            |+ PageSetting("noteskins.edit.columnlighttime", Slider.Percent(column_light_time))
                .Pos(270.0f)
                .Tooltip(Tooltip.Info("noteskins.edit.columnlighttime"))

            |+ PageSetting(
                "noteskins.edit.animationtime",
                Slider(
                    note_animation_time
                    |> Setting.trigger (fun v -> note_frames.Interval <- max 10.0 v)
                    |> Setting.f32,
                    Step = 1f
                )
            )
                .Pos(370.0f)
                .Tooltip(Tooltip.Info("noteskins.edit.animationtime"))

            |+ PageSetting("noteskins.edit.enableexplosions", Selector<_>.FromBool enable_explosions)
                .Pos(470.0f)
                .Tooltip(Tooltip.Info("noteskins.edit.enableexplosions"))
            |+ PageSetting(
                "noteskins.edit.explosionanimationtime",
                Slider(
                    explosion_animation_time
                    |> Setting.trigger (fun v -> explosion_frames.Interval <- max 10.0 v)
                    |> Setting.f32,
                    Step = 1f
                )
            )
                .Pos(540.0f)
                .Tooltip(Tooltip.Info("noteskins.edit.explosionanimationtime"))
            |+ PageSetting("noteskins.edit.explosionfadetime", Slider.Percent(explosion_fade_time))
                .Pos(610.0f)
                .Tooltip(Tooltip.Info("noteskins.edit.explosionfadetime"))
            |+ PageSetting("noteskins.edit.explosionscale", Slider.Percent(explosion_scale))
                .Pos(680.0f)
                .Tooltip(Tooltip.Info("noteskins.edit.explosionscale"))
            |+ PageSetting("noteskins.edit.explosionexpand", Slider.Percent(explosion_expand))
                .Pos(750.0f)
                .Tooltip(Tooltip.Info("noteskins.edit.explosionexpand"))
            |+ PageSetting("noteskins.edit.explodeonmiss", Selector<_>.FromBool(explosion_on_miss))
                .Pos(820.0f)
                .Tooltip(Tooltip.Info("noteskins.edit.explodeonmiss"))
            |+ PageSetting(
                "noteskins.edit.explosioncolors",
                Selector(
                    [| ExplosionColors.Column, "Column"; ExplosionColors.Judgements, "Judgements" |],
                    explosion_colors
                )
            )
                .Pos(890.0f)
                .Tooltip(Tooltip.Info("noteskins.edit.explosioncolors"))
        )

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        explosion_fade.Update elapsed_ms
        explosion_fade.Update elapsed_ms
        hold_explosion_fade.Update elapsed_ms
        explosion_frames.Update elapsed_ms
        note_frames.Update elapsed_ms
        test_events.Update elapsed_ms

        if test_event_i < test_events.Loops then
            test_event_i <- test_events.Loops
            explosion_fade.Value <- 1.0f

            if test_event_i % 2 = 0 then
                hold_explosion_fade.Target <- 0.0f
                hold_explosion_fade.Value <- 1.0f
            else
                hold_explosion_fade.Target <- 1.0f
                hold_explosion_fade.Snap()

    override this.Draw() =
        base.Draw()

        let COLUMN_WIDTH = 120.0f
        let mutable left = this.Bounds.Right - 50.0f - COLUMN_WIDTH * 2.0f
        let mutable bottom = this.Bounds.Bottom - 50.0f - COLUMN_WIDTH

        // draw note explosion example
        Draw.quad
            (Rect.Box(left, bottom - COLUMN_WIDTH, COLUMN_WIDTH, COLUMN_WIDTH) |> Quad.ofRect)
            (Quad.color Color.White)
            (Sprite.with_uv (note_frames.Loops, 0) receptor)

        if enable_explosions.Value then
            let threshold = max 0.0f (1.0f - explosion_fade_time.Value)

            let p =
                (explosion_fade.Value - threshold) / explosion_fade_time.Value
                |> min 1.0f
                |> max 0.0f

            let a = 255.0f * p |> int

            Draw.quad
                (Rect
                    .Box(left, bottom - COLUMN_WIDTH, COLUMN_WIDTH, COLUMN_WIDTH)
                    .Expand((explosion_scale.Value - 1.0f) * COLUMN_WIDTH * 0.5f)
                    .Expand(explosion_expand.Value * (1.0f - p) * COLUMN_WIDTH)
                 |> Quad.ofRect)
                (Quad.color (Color.White.O4a a))
                (Sprite.with_uv (explosion_frames.Loops, 0) noteexplosion)

        // draw hold explosion example
        bottom <- bottom - COLUMN_WIDTH * 2.0f

        Draw.quad
            (Rect.Box(left, bottom - COLUMN_WIDTH, COLUMN_WIDTH, COLUMN_WIDTH) |> Quad.ofRect)
            (Quad.color Color.White)
            (Sprite.with_uv (note_frames.Loops, 0) receptor)

        if enable_explosions.Value then
            let threshold = max 0.0f (1.0f - explosion_fade_time.Value)

            let p =
                (hold_explosion_fade.Value - threshold) / explosion_fade_time.Value
                |> min 1.0f
                |> max 0.0f

            let a = 255.0f * p |> int

            Draw.quad
                (Rect
                    .Box(left, bottom - COLUMN_WIDTH, COLUMN_WIDTH, COLUMN_WIDTH)
                    .Expand((explosion_scale.Value - 1.0f) * COLUMN_WIDTH * 0.5f)
                    .Expand(explosion_expand.Value * (1.0f - p) * COLUMN_WIDTH)
                 |> Quad.ofRect)
                (Quad.color (Color.White.O4a a))
                (Sprite.with_uv (explosion_frames.Loops, 0) holdexplosion)

        // draw note animation example
        bottom <- bottom - COLUMN_WIDTH * 2.0f

        Draw.quad
            (Rect.Box(left, bottom - COLUMN_WIDTH, COLUMN_WIDTH, COLUMN_WIDTH) |> Quad.ofRect)
            (Quad.color Color.White)
            (Sprite.with_uv (note_frames.Loops, 0) note)

        // draw column light example
        bottom <- bottom + COLUMN_WIDTH * 4.0f
        left <- left - COLUMN_WIDTH * 1.5f

        Draw.quad
            (Rect.Box(left, bottom - COLUMN_WIDTH, COLUMN_WIDTH, COLUMN_WIDTH) |> Quad.ofRect)
            (Quad.color Color.White)
            (Sprite.with_uv (note_frames.Loops, int hold_explosion_fade.Target) receptor)

        if enable_column_light.Value then
            let threshold = max 0.0f (1.0f - column_light_time.Value)

            let p =
                (hold_explosion_fade.Value - threshold) / column_light_time.Value
                |> min 1.0f
                |> max 0.0f

            let a = 255.0f * p |> int |> min 255 |> max 0

            Draw.sprite
                (Sprite.aligned_box_x
                    (left + COLUMN_WIDTH * 0.5f, bottom, 0.5f, 1.0f, COLUMN_WIDTH * p, 1.0f / p)
                    columnlighting)
                (Color.White.O4a a)
                columnlighting

    override this.Title = %"noteskins.edit.animations.name"

    override this.OnClose() =
        Noteskins.Current.save_config
            { Noteskins.Current.config with
                EnableColumnLight = enable_column_light.Value
                ColumnLightTime = column_light_time.Value
                AnimationFrameTime = note_animation_time.Value
                Explosions =
                    { Enable = enable_explosions.Value
                      Scale = explosion_scale.Value
                      FadeTime = explosion_fade_time.Value
                      ExpandAmount = explosion_expand.Value
                      ExplodeOnMiss = explosion_on_miss.Value
                      AnimationFrameTime = explosion_animation_time.Value
                      Colors = explosion_colors.Value } }
