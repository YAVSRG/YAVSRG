namespace Interlude.Features.OptionsMenu.Noteskins

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Prelude.Common
open Prelude.Data.Content
open Interlude.Content
open Interlude.Features
open Interlude.Utils
open Interlude.UI.Menu

type AnimationSettingsPage() as this =
    inherit Page()

    let data = Noteskins.Current.config

    let enable_column_Light = Setting.simple data.EnableColumnLight
    let column_light_time = Setting.percentf data.ColumnLightTime
    let note_animation_time = Setting.bounded data.AnimationFrameTime 10.0 1000.0

    // todo: setting to enable explosions
    // todo: replace column and explosion fade percentages with a real timing system
    // todo: option to disable fading of explosions
    // todo: add previews of explosions, hold explosions, column lights
    let explosion_animation_time = Setting.bounded data.Explosions.AnimationFrameTime 10.0 1000.0
    let explosion_fade_time = Setting.percentf data.Explosions.FadeTime
    let explosion_scale = Setting.bounded data.Explosions.Scale 0.5f 2.0f
    let explosion_expand = Setting.percentf data.Explosions.ExpandAmount
    let explosion_on_miss = Setting.simple data.Explosions.ExplodeOnMiss
    let explosion_colors = Setting.simple data.Explosions.Colors

    let note = getTexture "note"
    let noteexplosion = getTexture "noteexplosion"
    let holdexplosion = getTexture "holdexplosion"
    let receptor = getTexture "receptor"
    let note_frames = Animation.Counter (data.AnimationFrameTime)
    let explosion_frames = Animation.Counter (data.Explosions.AnimationFrameTime)
    let explosion_fade = Animation.Fade 0.0f

    do
        this.Content(
            column()
            |+ PageSetting("noteskins.edit.enablecolumnlight", Selector<_>.FromBool enable_column_Light)
                .Pos(200.0f)
                .Tooltip(Tooltip.Info("noteskins.edit.enablecolumnlight"))
            |+ PageSetting("noteskins.edit.columnlighttime", Slider.Percent(column_light_time))
                .Pos(270.0f)
                .Tooltip(Tooltip.Info("noteskins.edit.columnlighttime"))

            |+ PageSetting("noteskins.edit.animationtime", Slider(note_animation_time |> Setting.trigger (fun v -> note_frames.Interval <- max 10.0 v) |> Setting.f32, Step = 1f))
                .Pos(370.0f)
                .Tooltip(Tooltip.Info("noteskins.edit.animationtime"))

            |+ PageSetting("noteskins.edit.explosionanimationtime", Slider(explosion_animation_time |> Setting.trigger (fun v -> explosion_frames.Interval <- max 10.0 v) |> Setting.f32, Step = 1f))
                .Pos(470.0f)
                .Tooltip(Tooltip.Info("noteskins.edit.explosionanimationtime"))
            |+ PageSetting("noteskins.edit.explosionfadetime", Slider.Percent(explosion_fade_time))
                .Pos(540.0f)
                .Tooltip(Tooltip.Info("noteskins.edit.explosionfadetime"))
            |+ PageSetting("noteskins.edit.explosionscale", Slider.Percent(explosion_scale))
                .Pos(610.0f)
                .Tooltip(Tooltip.Info("noteskins.edit.explosionscale"))
            |+ PageSetting("noteskins.edit.explosionexpand", Slider.Percent(explosion_expand))
                .Pos(680.0f)
                .Tooltip(Tooltip.Info("noteskins.edit.explosionexpand"))
            |+ PageSetting("noteskins.edit.explodeonmiss", Selector<_>.FromBool(explosion_on_miss))
                .Pos(750.0f)
                .Tooltip(Tooltip.Info("noteskins.edit.explodeonmiss"))
            |+ PageSetting("noteskins.edit.explosioncolors", Selector([|ExplosionColors.Column, "Column"; ExplosionColors.Judgements, "Judgements"|], explosion_colors))
                .Pos(820.0f)
                .Tooltip(Tooltip.Info("noteskins.edit.explosioncolors"))
        )

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        explosion_fade.Update elapsedTime
        explosion_frames.Update elapsedTime
        note_frames.Update elapsedTime

        if (!|"skip").Tapped() then
            explosion_fade.Value <- 1.0f

    override this.Draw() =
        base.Draw()

        let COLUMN_WIDTH = 120.0f
        let mutable left = this.Bounds.Right - 50.0f - COLUMN_WIDTH * 2.0f
        let mutable bottom = this.Bounds.Bottom - 50.0f - COLUMN_WIDTH

        // draw note explosion example
        Draw.quad
            (
                Rect.Box(left, bottom - COLUMN_WIDTH, COLUMN_WIDTH, COLUMN_WIDTH)
                |> Quad.ofRect
            )
            (Quad.colorOf Color.White)
            (Sprite.gridUV(note_frames.Loops, 0) receptor)
        let threshold = max 0.0f (1.0f - explosion_fade_time.Value)
        let p = (explosion_fade.Value - threshold) / explosion_fade_time.Value |> min 1.0f |> max 0.0f
        let a = 255.0f * p |> int
        Draw.quad
            (
                Rect
                    .Box(left, bottom - COLUMN_WIDTH, COLUMN_WIDTH, COLUMN_WIDTH)
                    .Expand((explosion_scale.Value - 1.0f) * COLUMN_WIDTH * 0.5f)
                    .Expand(explosion_expand.Value * (1.0f - p) * COLUMN_WIDTH)
                |> Quad.ofRect
            )
            (Quad.colorOf (Color.White.O4a a))
            (Sprite.gridUV(explosion_frames.Loops, 0) noteexplosion)

    override this.Title = L"noteskins.edit.animations.name"
    override this.OnClose() =
        Noteskins.Current.changeConfig
            { Noteskins.Current.config with
                EnableColumnLight = enable_column_Light.Value
                ColumnLightTime = column_light_time.Value
                AnimationFrameTime = note_animation_time.Value
                Explosions =
                    {
                        Scale = explosion_scale.Value
                        FadeTime = explosion_fade_time.Value
                        ExpandAmount = explosion_expand.Value
                        ExplodeOnMiss = explosion_on_miss.Value
                        AnimationFrameTime = explosion_animation_time.Value
                        Colors = explosion_colors.Value
                    }
            }