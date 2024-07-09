namespace Interlude.Features.Skins.EditNoteskin

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Skins.Noteskins
open Interlude.Content
open Interlude.UI

type AnimationSettingsPage() =
    inherit Page()

    let data = Content.NoteskinConfig

    let note = Content.Texture "note"
    let noteexplosion = Content.Texture "noteexplosion"
    let holdexplosion = Content.Texture "holdexplosion"
    let releaseexplosion = Content.Texture "releaseexplosion"
    let receptor = Content.Texture "receptor"
    let columnlighting = Content.Texture "receptorlighting"

    let mutable holding = false
    let test_events = Animation.Counter 1000.0
    let f_note = Animation.Counter(data.AnimationFrameTime)

    let t_columnlight = Animation.Delay(data.ColumnLightDuration)

    let f_note_ex = Animation.Counter(data.NoteExplosionSettings.AnimationFrameTime)
    let t_note_ex = Animation.Delay(data.NoteExplosionSettings.Duration)

    let f_hold_ex = Animation.Counter(data.HoldExplosionSettings.AnimationFrameTime)
    let t_hold_ex = Animation.Delay(data.HoldExplosionSettings.Duration)

    let note_animation_time =
        Setting.bounded data.AnimationFrameTime 10.0 1000.0
        |> Setting.round 0
        |> Setting.trigger f_note.set_Interval

    let enable_column_light = Setting.simple data.EnableColumnLight

    let column_light_duration =
        Setting.bounded data.ColumnLightDuration 0.0 1000.0
        |> Setting.round 0
        |> Setting.trigger t_columnlight.set_Interval

    let enable_explosions = Setting.simple data.UseExplosions

    let explosion_frame_time_note =
        Setting.bounded data.NoteExplosionSettings.AnimationFrameTime 10.0 1000.0
        |> Setting.round 0
        |> Setting.trigger f_note_ex.set_Interval

    let explosion_colors_note = Setting.simple data.NoteExplosionSettings.Colors

    let explosion_offset_note =
        Setting.bounded data.NoteExplosionSettings.Offset -1.0f 1.0f

    let explosion_builtin_note =
        Setting.simple data.NoteExplosionSettings.UseBuiltInAnimation

    let explosion_duration_note =
        Setting.bounded data.NoteExplosionSettings.Duration 50.0 1000
        |> Setting.round 0
        |> Setting.trigger t_note_ex.set_Interval

    let explosion_scale_note =
        Setting.bounded data.NoteExplosionSettings.Scale 0.5f 5.0f

    let explosion_expand_note = Setting.percentf data.NoteExplosionSettings.ExpandAmount

    let explosion_frame_time_hold =
        Setting.bounded data.HoldExplosionSettings.AnimationFrameTime 10.0 1000.0
        |> Setting.round 0
        |> Setting.trigger f_hold_ex.set_Interval

    let explosion_colors_hold = Setting.simple data.HoldExplosionSettings.Colors

    let explosion_offset_hold =
        Setting.bounded data.HoldExplosionSettings.Offset -1.0f 1.0f

    let explosion_hold_use_release =
        Setting.simple data.HoldExplosionSettings.UseReleaseExplosion

    let explosion_builtin_release =
        Setting.simple data.HoldExplosionSettings.ReleaseUseBuiltInAnimation

    let explosion_duration_hold =
        Setting.bounded data.HoldExplosionSettings.Duration 50.0 1000
        |> Setting.round 0
        |> Setting.trigger t_hold_ex.set_Interval

    let explosion_scale_hold =
        Setting.bounded data.HoldExplosionSettings.Scale 0.5f 5.0f

    let explosion_expand_hold = Setting.percentf data.HoldExplosionSettings.ExpandAmount

    override this.Content() =
        let general_tab =
            NavigationContainer.Column(WrapNavigation = false)
            |+ PageSetting(%"noteskin.enablecolumnlight", Checkbox enable_column_light)
                .Help(Help.Info("noteskin.enablecolumnlight"))
                .Pos(0)
            |+ PageSetting(
                %"noteskin.columnlighttime",
                Slider(column_light_duration |> Setting.f32, Step = 1f)
            )
                .Help(Help.Info("noteskin.columnlighttime"))
                .Pos(2)
            |+ PageSetting(%"noteskin.animationtime", Slider(note_animation_time |> Setting.f32, Step = 1f))
                .Help(Help.Info("noteskin.animationtime"))
                .Pos(5)
            |+ PageSetting(%"noteskin.enableexplosions", Checkbox enable_explosions)
                .Help(Help.Info("noteskin.enableexplosions"))
                .Pos(8)

        let note_explosion_tab =
            NavigationContainer.Column(WrapNavigation = false)
            |+ PageSetting(
                %"noteskin.explosionanimationtime",
                Slider(explosion_frame_time_note |> Setting.f32, Step = 1f)
            )
                .Help(Help.Info("noteskin.explosionanimationtime"))
                .Pos(0)
            |+ PageSetting(
                %"noteskin.explosioncolors",
                SelectDropdown(
                    [| ExplosionColors.Note, "Note"; ExplosionColors.Judgements, "Judgements" |],
                    explosion_colors_note
                )
            )
                .Help(Help.Info("noteskin.explosioncolors"))
                .Pos(2)
            |+ PageSetting(%"noteskin.explosionoffset", Slider.Percent(explosion_offset_note))
                .Help(Help.Info("noteskin.explosionoffset"))
                .Pos(4)
            |+ PageSetting(%"noteskin.explosionscale", Slider.Percent(explosion_scale_note))
                .Help(Help.Info("noteskin.explosionscale"))
                .Pos(6)
            |+ PageSetting(%"noteskin.usebuiltinanimation", Checkbox explosion_builtin_note)
                .Help(Help.Info("noteskin.usebuiltinanimation"))
                .Pos(8)
            |+ PageSetting(
                %"noteskin.explosionduration",
                Slider(explosion_duration_note |> Setting.f32, Step = 1f)
            )
                .Help(Help.Info("noteskin.explosionduration"))
                .Pos(10)
                .Conditional(explosion_builtin_note.Get)
            |+ PageSetting(%"noteskin.explosionexpand", Slider.Percent(explosion_expand_note))
                .Help(Help.Info("noteskin.explosionexpand"))
                .Pos(12)
                .Conditional(explosion_builtin_note.Get)

        let hold_explosion_tab =
            NavigationContainer.Column(WrapNavigation = false)
            |+ PageSetting(
                %"noteskin.explosionanimationtime",
                Slider(explosion_frame_time_hold |> Setting.f32, Step = 1f)
            )
                .Help(Help.Info("noteskin.explosionanimationtime"))
                .Pos(0)
            |+ PageSetting(
                %"noteskin.explosioncolors",
                SelectDropdown(
                    [| ExplosionColors.Note, "Note"; ExplosionColors.Judgements, "Judgements" |],
                    explosion_colors_hold
                )
            )
                .Help(Help.Info("noteskin.explosioncolors"))
                .Pos(2)
            |+ PageSetting(%"noteskin.explosionoffset", Slider.Percent(explosion_offset_hold))
                .Help(Help.Info("noteskin.explosionoffset"))
                .Pos(4)
            |+ PageSetting(%"noteskin.explosionscale", Slider.Percent(explosion_scale_hold))
                .Help(Help.Info("noteskin.explosionscale"))
                .Pos(6)
            |+ PageSetting(%"noteskin.usereleaseanimation", Checkbox explosion_hold_use_release)
                .Help(Help.Info("noteskin.usereleaseanimation"))
                .Pos(8)
            |+ PageSetting(%"noteskin.usebuiltinanimation", Checkbox explosion_builtin_release)
                .Help(Help.Info("noteskin.usebuiltinanimation"))
                .Pos(10)
                .Conditional(explosion_hold_use_release.Get)
            |+ PageSetting(
                %"noteskin.explosionduration",
                Slider(explosion_duration_hold |> Setting.f32, Step = 1f)
            )
                .Help(Help.Info("noteskin.explosionduration"))
                .Pos(12)
                .Conditional(fun () -> explosion_builtin_release.Value || not explosion_hold_use_release.Value)
            |+ PageSetting(%"noteskin.explosionexpand", Slider.Percent(explosion_expand_hold))
                .Help(Help.Info("noteskin.explosionexpand"))
                .Pos(14)
                .Conditional(fun () -> explosion_builtin_release.Value || not explosion_hold_use_release.Value)

        let tabs = SwapContainer(general_tab, Position = Position.Margin(PRETTY_MARGIN_X, PRETTY_MARGIN_Y).TrimTop(100.0f))

        let tab_buttons =
            RadioButtons.create_tabs
                {
                    Setting = Setting.make tabs.set_Current tabs.get_Current
                    Options =
                        [|
                            general_tab, %"noteskin.general", K false
                            note_explosion_tab, %"noteskin.note_explosions", (fun () -> not enable_explosions.Value)
                            hold_explosion_tab, %"noteskin.hold_explosions", (fun () -> not enable_explosions.Value)
                        |]
                    Height = 50.0f
                }

        tab_buttons.Position <- pretty_pos(0, 2, PageWidth.Normal).Translate(PRETTY_MARGIN_X, PRETTY_MARGIN_Y)

        NavigationContainer.Column()
        |+ tab_buttons
        |+ tabs
        :> Widget

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        f_note.Update elapsed_ms
        f_note_ex.Update elapsed_ms
        f_hold_ex.Update elapsed_ms

        t_columnlight.Update elapsed_ms
        t_note_ex.Update elapsed_ms
        t_hold_ex.Update elapsed_ms
        t_columnlight.Update elapsed_ms

        test_events.Update elapsed_ms

        if holding <> (test_events.Loops % 2 = 0) then
            holding <- test_events.Loops % 2 = 0

            if holding then
                t_note_ex.Reset()
                f_note_ex.Reset()
                f_hold_ex.Reset()
            else
                t_columnlight.Reset()
                t_hold_ex.Reset()
                f_hold_ex.Reset()

    override this.Draw() =
        base.Draw()

        let COLUMN_WIDTH = 120.0f
        let mutable left = this.Bounds.Right - 50.0f - COLUMN_WIDTH * 2.0f
        let mutable bottom = this.Bounds.Bottom - 50.0f - COLUMN_WIDTH

        // draw note explosion example
        Draw.quad
            (Rect.Box(left, bottom - COLUMN_WIDTH, COLUMN_WIDTH, COLUMN_WIDTH / receptor.AspectRatio).AsQuad)
            Color.White.AsQuad
            (Sprite.pick_texture (f_note.Loops, 0) receptor)

        if enable_explosions.Value then

            let percent_remaining =
                if explosion_builtin_note.Value then
                    1.0 - (t_note_ex.Time / explosion_duration_note.Value)
                    |> min 1.0
                    |> max 0.0
                    |> float32
                else if f_note_ex.Loops > noteexplosion.Columns then
                    0.0f
                else
                    1.0f

            let a = 255.0f * percent_remaining |> int

            Draw.quad
                (Rect
                    .Box(left, bottom - COLUMN_WIDTH, COLUMN_WIDTH, COLUMN_WIDTH)
                    .Expand((explosion_scale_note.Value - 1.0f) * COLUMN_WIDTH * 0.5f)
                    .Expand(explosion_expand_note.Value * (1.0f - percent_remaining) * COLUMN_WIDTH)
                    .Translate(0.0f, -COLUMN_WIDTH * explosion_offset_note.Value)
                    .AsQuad)
                (Color.White.O4a a).AsQuad
                (Sprite.pick_texture (f_note_ex.Loops, 0) noteexplosion)

        // draw hold explosion example
        bottom <- bottom - COLUMN_WIDTH * 2.0f

        Draw.quad
            (Rect.Box(left, bottom - COLUMN_WIDTH, COLUMN_WIDTH, COLUMN_WIDTH / receptor.AspectRatio).AsQuad)
            Color.White.AsQuad
            (Sprite.pick_texture (f_note.Loops, (if holding then 1 else 0)) receptor)

        if enable_explosions.Value then

            if not explosion_hold_use_release.Value || holding then

                let percent_remaining =
                    if holding then
                        1.0f
                    else
                        1.0 - (t_hold_ex.Time / explosion_duration_hold.Value)
                        |> min 1.0
                        |> max 0.0
                        |> float32

                let a = 255.0f * percent_remaining |> int

                Draw.quad
                    (Rect
                        .Box(left, bottom - COLUMN_WIDTH, COLUMN_WIDTH, COLUMN_WIDTH)
                        .Expand((explosion_scale_hold.Value - 1.0f) * COLUMN_WIDTH * 0.5f)
                        .Expand(explosion_expand_hold.Value * (1.0f - percent_remaining) * COLUMN_WIDTH)
                        .Translate(0.0f, -COLUMN_WIDTH * explosion_offset_hold.Value)
                        .AsQuad)
                    (Color.White.O4a a).AsQuad
                    (Sprite.pick_texture (f_hold_ex.Loops, 0) holdexplosion)

            else

                let percent_remaining =
                    if explosion_builtin_release.Value then
                        1.0 - (t_hold_ex.Time / explosion_duration_hold.Value)
                        |> min 1.0
                        |> max 0.0
                        |> float32
                    else if f_hold_ex.Loops > releaseexplosion.Columns then
                        0.0f
                    else
                        1.0f

                let a = 255.0f * percent_remaining |> int

                Draw.quad
                    (Rect
                        .Box(left, bottom - COLUMN_WIDTH, COLUMN_WIDTH, COLUMN_WIDTH)
                        .Expand((explosion_scale_hold.Value - 1.0f) * COLUMN_WIDTH * 0.5f)
                        .Expand(explosion_expand_hold.Value * (1.0f - percent_remaining) * COLUMN_WIDTH)
                        .Translate(0.0f, -COLUMN_WIDTH * explosion_offset_hold.Value)
                        .AsQuad)
                    (Color.White.O4a a).AsQuad
                    (Sprite.pick_texture (f_hold_ex.Loops, 0) releaseexplosion)

        // draw note animation example
        bottom <- bottom - COLUMN_WIDTH * 2.0f

        Draw.quad
            (Rect.Box(left, bottom - COLUMN_WIDTH, COLUMN_WIDTH, COLUMN_WIDTH).AsQuad)
            Color.White.AsQuad
            (Sprite.pick_texture (f_note.Loops, 0) note)

        // draw column light example
        bottom <- bottom + COLUMN_WIDTH * 4.0f
        left <- left - COLUMN_WIDTH * 1.5f

        Draw.quad
            (Rect.Box(left, bottom - COLUMN_WIDTH, COLUMN_WIDTH, COLUMN_WIDTH / receptor.AspectRatio).AsQuad)
            Color.White.AsQuad
            (Sprite.pick_texture (f_note.Loops, (if holding then 1 else 0)) receptor)

        if enable_column_light.Value then

            let percent_remaining =
                if holding then
                    1.0f
                else
                    1.0 - (t_columnlight.Time / t_columnlight.Interval)
                    |> min 1.0
                    |> max 0.0
                    |> float32

            let a = 255.0f * percent_remaining |> int |> min 255 |> max 0

            Draw.sprite
                (Sprite.aligned_box_x
                    (left + COLUMN_WIDTH * 0.5f,
                     bottom,
                     0.5f,
                     1.0f,
                     COLUMN_WIDTH * percent_remaining,
                     1.0f / percent_remaining)
                    columnlighting)
                (Color.White.O4a a)
                columnlighting

    override this.Title = %"noteskin.animations"

    override this.OnClose() =
        Skins.save_noteskin_config
            { Content.NoteskinConfig with
                EnableColumnLight = enable_column_light.Value
                ColumnLightDuration = column_light_duration.Value
                AnimationFrameTime = note_animation_time.Value
                UseExplosions = enable_explosions.Value
                NoteExplosionSettings =
                    {
                        AnimationFrameTime = explosion_frame_time_note.Value
                        Scale = explosion_scale_note.Value
                        Colors = explosion_colors_note.Value
                        Offset = explosion_offset_note.Value
                        UseBuiltInAnimation = explosion_builtin_note.Value
                        Duration = explosion_duration_note.Value
                        ExpandAmount = explosion_expand_note.Value
                    }
                HoldExplosionSettings =
                    {
                        AnimationFrameTime = explosion_frame_time_hold.Value
                        Scale = explosion_scale_hold.Value
                        Colors = explosion_colors_hold.Value
                        Offset = explosion_offset_hold.Value
                        UseReleaseExplosion = explosion_hold_use_release.Value
                        ReleaseUseBuiltInAnimation = explosion_builtin_release.Value
                        Duration = explosion_duration_hold.Value
                        ExpandAmount = explosion_expand_hold.Value
                    }
            }
