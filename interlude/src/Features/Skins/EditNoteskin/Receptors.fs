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

    (* Preview *)

    let note = Content.Texture "note"
    let noteexplosion = Content.Texture "noteexplosion"
    let holdexplosion = Content.Texture "holdexplosion"
    let releaseexplosion = Content.Texture "releaseexplosion"
    let receptor = Content.Texture "receptor"
    let judgementline = Content.Texture "judgementline"
    let columnlighting = Content.Texture "receptorlighting"

    let mutable holding = false
    let test_events = Animation.Counter 1000.0
    let f_note = Animation.Counter(data.AnimationFrameTime)

    let t_columnlight = Animation.Delay(data.ColumnLightDuration)

    let f_note_ex = Animation.Counter(data.NoteExplosionSettings.AnimationFrameTime)
    let t_note_ex = Animation.Delay(data.NoteExplosionSettings.Duration)

    let f_hold_ex = Animation.Counter(data.HoldExplosionSettings.AnimationFrameTime)
    let t_hold_ex = Animation.Delay(data.HoldExplosionSettings.Duration)

    (* Receptors *)

    let enable_receptors = Setting.simple data.UseReceptors
    let receptor_style = Setting.simple data.ReceptorStyle
    let receptor_offset = Setting.bounded data.ReceptorOffset -1.0f 1.0f
    let notes_under_receptors = Setting.simple data.NotesUnderReceptors

    let receptors_tab =
        NavigationContainer.Column(WrapNavigation = false)
        |+ PageSetting(%"noteskin.enable_receptors", Checkbox enable_receptors)
            .Pos(0)
        |+ PageSetting(
            %"noteskin.receptorstyle",
            SelectDropdown(
                [|
                    ReceptorStyle.Receptors, %"noteskin.receptorstyle.receptors"
                    ReceptorStyle.Keys, %"noteskin.receptorstyle.keys"
                |],
                receptor_style
            )
        )
            .Help(Help.Info("noteskin.receptorstyle"))
            .Pos(2)
            .Conditional(enable_receptors.Get)
        |+ PageSetting(
            %"noteskin.receptor_offset",
            Slider.Percent(receptor_offset)
        )
            .Help(Help.Info("noteskin.receptor_offset"))
            .Pos(4)
            .Conditional(enable_receptors.Get)
        |+ PageSetting(
            %"noteskin.notes_under_receptors",
            Checkbox notes_under_receptors
        )
            .Pos(6)

    (* Judgement line *)

    let enable_judgement_line = Setting.simple data.UseJudgementLine
    let judgement_line_scale = Setting.bounded data.JudgementLineScale 0.1f 3f
    let judgement_line_offset = Setting.bounded data.JudgementLineOffset -1.0f 1.0f
    
    let judgement_line_tab =
        NavigationContainer.Column(WrapNavigation = false)
        |+ PageSetting(
            %"noteskin.enable_judgement_line",
            Checkbox enable_judgement_line
        )
            .Pos(0)
        |+ PageSetting(
            %"noteskin.judgement_line_scale",
            Slider.Percent(judgement_line_scale)
        )
            .Pos(2)
            .Conditional(enable_judgement_line.Get)
        |+ PageSetting(
            %"noteskin.judgement_line_offset",
            Slider.Percent(judgement_line_offset)
        )
            .Help(Help.Info("noteskin.judgement_line_offset"))
            .Pos(4)
            .Conditional(enable_judgement_line.Get)

    (* Column lighting *)

    let enable_column_light = Setting.simple data.EnableColumnLight
    let column_light_offset = Setting.bounded data.ColumnLightOffset -1.0f 1.0f
    let column_light_duration =
        Setting.bounded data.ColumnLightDuration 0.0 1000.0
        |> Setting.round 0
        |> Setting.trigger t_columnlight.set_Interval

    let column_light_tab =
        NavigationContainer.Column(WrapNavigation = false)
        |+ PageSetting(%"noteskin.enablecolumnlight", Checkbox enable_column_light)
            .Help(Help.Info("noteskin.enablecolumnlight"))
            .Pos(0)
        |+ PageSetting(%"noteskin.columnlightoffset", Slider.Percent(column_light_offset))
            .Help(Help.Info("noteskin.columnlightoffset"))
            .Pos(2)
            .Conditional(enable_column_light.Get)
        |+ PageSetting(
            %"noteskin.columnlighttime",
            Slider(column_light_duration |> Setting.f32, Step = 1f)
        )
            .Help(Help.Info("noteskin.columnlighttime"))
            .Pos(4)
            .Conditional(enable_column_light.Get)
    
    (* Explosions *)

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

    let note_explosions_subtab =
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

    let hold_explosions_subtab =
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

    let explosions_tab =
        let subtabs = SwapContainer(note_explosions_subtab, Position = Position.ShrinkT(150.0f))
        let subtab_buttons =
            RadioButtons.create_tabs
                {
                    Setting = Setting.make subtabs.set_Current subtabs.get_Current
                    Options =
                        [|
                            note_explosions_subtab, %"noteskin.note_explosions", K false
                            hold_explosions_subtab, %"noteskin.hold_explosions", K false
                        |]
                    Height = 50.0f
                }

        subtab_buttons.Position <- pretty_pos(2, 2, PageWidth.Normal).TranslateY(20.0f)

        NavigationContainer.Column()
        |+ PageSetting(%"noteskin.enable_explosions", Checkbox enable_explosions)
            .Pos(0)
        |+ subtab_buttons
            .Conditional(enable_explosions.Get)
        |+ subtabs
            .Conditional(enable_explosions.Get)
        :> Widget

    override this.Content() =
        let tabs = SwapContainer(receptors_tab, Position = Position.Shrink(PRETTY_MARGIN_X, PRETTY_MARGIN_Y).ShrinkT(100.0f))

        let tab_buttons =
            RadioButtons.create_tabs
                {
                    Setting = Setting.make tabs.set_Current tabs.get_Current
                    Options =
                        [|
                            receptors_tab, %"noteskin.receptors", K false
                            judgement_line_tab, %"noteskin.judgement_line", K false
                            column_light_tab, %"noteskin.column_lighting", K false
                            explosions_tab, %"noteskin.explosions", K false
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
        let preview_width =
            1.0f
            + (if enable_column_light.Value then 1.0f else 0.0f)
            + (if enable_explosions.Value then 2.0f else 0.0f)

        let center = this.Bounds.Right - (this.Bounds.Width - PRETTYWIDTH - PRETTY_MARGIN_X * 2.0f) * 0.5f

        let mutable left = center - preview_width * COLUMN_WIDTH * 0.5f
        let position = this.Bounds.Top + this.Bounds.Height * 0.6f

        if notes_under_receptors.Value then
            Draw.quad
                (Rect
                    .Box(left, position - COLUMN_WIDTH, COLUMN_WIDTH, COLUMN_WIDTH)
                    .AsQuad)
                Color.White.AsQuad
                (Sprite.pick_texture (f_note.Loops, 0) note)

        if enable_judgement_line.Value then
            Draw.quad
                (Rect
                    .Box(left, position - COLUMN_WIDTH, preview_width * COLUMN_WIDTH, COLUMN_WIDTH)
                    .ExpandPercentY(judgement_line_scale.Value - 1.0f)
                    .TranslateY(-COLUMN_WIDTH * judgement_line_offset.Value)
                    .AsQuad)
                Color.White.AsQuad
                (Sprite.pick_texture (f_note.Loops, 0) judgementline)

        let receptor() =
            if enable_receptors.Value then
                Draw.quad
                    (Rect
                        .Box(left, position - COLUMN_WIDTH, COLUMN_WIDTH, COLUMN_WIDTH / receptor.AspectRatio)
                        .TranslateY(-COLUMN_WIDTH * receptor_offset.Value)
                        .AsQuad)
                    Color.White.AsQuad
                    (Sprite.pick_texture (f_note.Loops, (if holding then 1 else 0)) receptor)

        receptor()
        if not notes_under_receptors.Value then
            Draw.quad
                (Rect
                    .Box(left, position - COLUMN_WIDTH, COLUMN_WIDTH, COLUMN_WIDTH)
                    .AsQuad)
                Color.White.AsQuad
                (Sprite.pick_texture (f_note.Loops, 0) note)

        if enable_column_light.Value then
            left <- left + COLUMN_WIDTH
            receptor()

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
                     position - COLUMN_WIDTH * column_light_offset.Value,
                     0.5f,
                     1.0f,
                     COLUMN_WIDTH * percent_remaining,
                     1.0f / percent_remaining)
                    columnlighting)
                (Color.White.O4a a)
                columnlighting

        if enable_explosions.Value then
            left <- left + COLUMN_WIDTH
            receptor()

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
                    .Box(left, position - COLUMN_WIDTH, COLUMN_WIDTH, COLUMN_WIDTH)
                    .Expand((explosion_scale_note.Value - 1.0f) * COLUMN_WIDTH * 0.5f)
                    .Expand(explosion_expand_note.Value * (1.0f - percent_remaining) * COLUMN_WIDTH)
                    .Translate(0.0f, -COLUMN_WIDTH * explosion_offset_note.Value)
                    .AsQuad)
                (Color.White.O4a a).AsQuad
                (Sprite.pick_texture (f_note_ex.Loops, 0) noteexplosion)

            left <- left + COLUMN_WIDTH
            receptor()

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
                        .Box(left, position - COLUMN_WIDTH, COLUMN_WIDTH, COLUMN_WIDTH)
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
                        .Box(left, position - COLUMN_WIDTH, COLUMN_WIDTH, COLUMN_WIDTH)
                        .Expand((explosion_scale_hold.Value - 1.0f) * COLUMN_WIDTH * 0.5f)
                        .Expand(explosion_expand_hold.Value * (1.0f - percent_remaining) * COLUMN_WIDTH)
                        .Translate(0.0f, -COLUMN_WIDTH * explosion_offset_hold.Value)
                        .AsQuad)
                    (Color.White.O4a a).AsQuad
                    (Sprite.pick_texture (f_hold_ex.Loops, 0) releaseexplosion)

    override this.Title = %"noteskin.animations"

    override this.OnClose() =
        Skins.save_noteskin_config
            { Content.NoteskinConfig with
                NotesUnderReceptors = notes_under_receptors.Value

                UseReceptors = enable_receptors.Value
                ReceptorStyle = receptor_style.Value
                ReceptorOffset = receptor_offset.Value

                UseJudgementLine = enable_judgement_line.Value
                JudgementLineScale = judgement_line_scale.Value
                JudgementLineOffset = judgement_line_offset.Value

                EnableColumnLight = enable_column_light.Value
                ColumnLightOffset = column_light_offset.Value
                ColumnLightDuration = column_light_duration.Value

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
