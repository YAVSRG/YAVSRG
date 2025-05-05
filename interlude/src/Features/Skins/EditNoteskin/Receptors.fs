namespace Interlude.Features.Skins.EditNoteskin

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Prelude
open Prelude.Skins.Noteskins
open Interlude.Content
open Interlude.UI
open Interlude.Options
open Interlude.Features.Gameplay

type ReceptorColorPicker(color: Setting<int>) =
    inherit Container(NodeType.Leaf)

    let sprite = Content.Texture "receptor"
    let n = sprite.Rows / 2

    let fd () =
        Setting.app (fun x -> (x + 1) % n) color
        Style.click.Play()

    let bk () =
        Setting.app (fun x -> (x + n - 1) % n) color
        Style.click.Play()

    override this.Init(parent: Widget) =
        this
        |* MouseListener()
            .SelectOnClick(this, fd)
            .FocusOnHover(this)
            .OnRightClick(fun () ->
                this.Select true
                bk()
            )

        base.Init parent

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Draw() =
        base.Draw()

        if this.Selected then
            Render.rect (this.Bounds.Expand(Style.PADDING)) Colors.pink_accent.O2
        elif this.Focused then
            Render.rect (this.Bounds.Expand(Style.PADDING)) Colors.yellow_accent.O2

        Render.tex_quad (this.Bounds.SliceY(this.Bounds.Width).SlicePercentT(1.0f / sprite.AspectRatio).AsQuad) Color.White.AsQuad (Sprite.pick_texture (0, 1 + color.Value * 2) sprite)

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if this.Selected then
            if (%%"up").Pressed() then
                fd ()
            elif (%%"down").Pressed() then
                bk ()
            elif (%%"left").Pressed() then
                bk ()
            elif (%%"right").Pressed() then
                fd ()

type ColumnLightColorPicker(color: Setting<int>) =
    inherit Container(NodeType.Leaf)

    let sprite = Content.Texture "receptorlighting"
    let n = sprite.Rows

    let fd () =
        Setting.app (fun x -> (x + 1) % n) color
        Style.click.Play()

    let bk () =
        Setting.app (fun x -> (x + n - 1) % n) color
        Style.click.Play()

    override this.Init(parent: Widget) =
        this
        |* MouseListener()
            .SelectOnClick(this, fd)
            .FocusOnHover(this)
            .OnRightClick(fun () ->
                this.Select true
                bk()
            )

        base.Init parent

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Draw() =
        base.Draw()

        if this.Selected then
            Render.rect (this.Bounds.Expand(Style.PADDING)) Colors.pink_accent.O2
        elif this.Focused then
            Render.rect (this.Bounds.Expand(Style.PADDING)) Colors.yellow_accent.O2

        Render.tex_quad (this.Bounds.SliceY(this.Bounds.Width).ExpandPercentY((1.0f / sprite.AspectRatio |> min 2.0f) - 1.0f).AsQuad) Color.White.AsQuad (Sprite.pick_texture (0, color.Value) sprite)

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if this.Selected then
            if (%%"up").Pressed() then
                fd ()
            elif (%%"down").Pressed() then
                bk ()
            elif (%%"left").Pressed() then
                bk ()
            elif (%%"right").Pressed() then
                fd ()

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
    let f_note = Animation.Counter(float data.AnimationFrameTime)

    let t_columnlight = Animation.Delay(data.ColumnLightDuration)

    let f_note_ex = Animation.Counter(float data.NoteExplosionSettings.AnimationFrameTime)
    let t_note_ex = Animation.Delay(float data.NoteExplosionSettings.Duration)

    let f_hold_ex = Animation.Counter(float data.HoldExplosionSettings.AnimationFrameTime)
    let t_hold_ex = Animation.Delay(float data.HoldExplosionSettings.Duration)

    let NOTE_SCALE = (PAGE_ITEM_WIDTH - PAGE_LABEL_WIDTH) / 10.0f

    (* Receptors *)

    let enable_receptors = Setting.simple data.UseReceptors
    let receptor_style = Setting.simple data.ReceptorStyle
    let receptor_offset = data.ReceptorOffset |> Setting.bounded (-10.0f, 10.0f)
    let notes_under_receptors = Setting.simple data.NotesUnderReceptors

    let receptors_tab =

        let keymode: Setting<Keymode> = Setting.simple <| SelectedChart.keymode ()

        let receptor_colors = data.ReceptorColors
        let color_picker keycount i =
            let k = int keycount - 3

            Setting.make (fun v -> receptor_colors.[k].[i] <- v) (fun () -> receptor_colors.[k].[i])

        let colors, refresh_colors =
            refreshable_row
                (fun () -> int keymode.Value)
                (fun i k ->
                    let x = NOTE_SCALE * -0.5f * float32 k
                    let n = float32 i

                    ReceptorColorPicker(color_picker keymode.Value i)
                        .Position(
                            { Position.DEFAULT with
                                Left = 0.5f %+ (x + NOTE_SCALE * n)
                                Right = 0.5f %+ (x + NOTE_SCALE * n + NOTE_SCALE)
                            }
                        )
                )

        NavigationContainer.Column(WrapNavigation = false)
        |+ PageSetting(%"noteskin.enable_receptors", Checkbox enable_receptors)
            .Pos(0)
        |+ PageSetting(
            %"noteskin.notes_under_receptors",
            Checkbox notes_under_receptors
        )
            .Pos(2)
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
            .Pos(4)
            .Conditional(enable_receptors.Get)
        |+ PageSetting(
            %"noteskin.receptor_offset",
            Slider.Percent(receptor_offset)
        )
            .Help(Help.Info("noteskin.receptor_offset"))
            .Pos(6)
            .Conditional(enable_receptors.Get)
        |+ PageSetting(
            %"generic.keymode",
            SelectDropdown.FromEnum(keymode |> Setting.trigger (fun _ -> refresh_colors()))
        )
            .Pos(9)
            .Conditional(fun () -> enable_receptors.Value && receptor.Rows > 2)
        |+ PageSetting(%"noteskin.receptor_colors", colors)
            .Pos(11, 3, PageWidth.Normal)
            .Conditional(fun () -> enable_receptors.Value && receptor.Rows > 2)

    (* Judgement line *)

    let enable_judgement_line = Setting.simple data.UseJudgementLine
    let judgement_line_scale = data.JudgementLineScale |> Setting.bounded (0.1f, 3f)
    let judgement_line_offset = data.JudgementLineOffset |> Setting.bounded(-1.0f, 1.0f)

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
    let column_light_offset = data.ColumnLightOffset |> Setting.bounded (-1.0f, 1.0f)
    let column_light_duration =
        data.ColumnLightDuration
        |> Setting.bounded (0.0, 1000.0)
        |> Setting.round 0
        |> Setting.trigger t_columnlight.set_Interval

    let column_light_tab =

        let keymode: Setting<Keymode> = Setting.simple <| SelectedChart.keymode ()

        let column_light_colors = data.ColumnLightColors
        let color_picker keycount i =
            let k = int keycount - 3

            Setting.make (fun v -> column_light_colors.[k].[i] <- v) (fun () -> column_light_colors.[k].[i])

        let colors, refresh_colors =
            refreshable_row
                (fun () -> int keymode.Value)
                (fun i k ->
                    let x = NOTE_SCALE * -0.5f * float32 k
                    let n = float32 i

                    ColumnLightColorPicker(color_picker keymode.Value i)
                        .Position(
                            { Position.DEFAULT with
                                Left = 0.5f %+ (x + NOTE_SCALE * n)
                                Right = 0.5f %+ (x + NOTE_SCALE * n + NOTE_SCALE)
                            }
                        )
                )

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
        |+ PageSetting(
            %"generic.keymode",
            SelectDropdown.FromEnum(keymode |> Setting.trigger (fun _ -> refresh_colors()))
        )
            .Pos(7)
            .Conditional(fun () -> columnlighting.Rows > 1 && enable_column_light.Value)
        |+ PageSetting(%"noteskin.column_light_colors", colors)
            .Pos(9, 3, PageWidth.Normal)
            .Conditional(fun () -> columnlighting.Rows > 1 && enable_column_light.Value)

    (* Explosions *)

    let enable_explosions = Setting.simple data.UseExplosions

    let explosion_frame_time_note =
        data.NoteExplosionSettings.AnimationFrameTime
        |> Setting.bounded (10.0f<ms / rate>, 1000.0f<ms / rate>)
        |> Setting.roundf_uom 0
        |> Setting.trigger (float >> f_note_ex.set_Interval)

    let explosion_colors_note = Setting.simple data.NoteExplosionSettings.Colors

    let explosion_offset_note =
        data.NoteExplosionSettings.Offset
        |> Setting.bounded (-1.0f, 1.0f)

    let explosion_builtin_note =
        Setting.simple data.NoteExplosionSettings.UseBuiltInAnimation

    let explosion_duration_note =
        data.NoteExplosionSettings.Duration
        |> Setting.bounded (50.0f<ms / rate>, 1000f<ms / rate>)
        |> Setting.roundf_uom 0
        |> Setting.trigger (float >> t_note_ex.set_Interval)

    let explosion_scale_note =
        data.NoteExplosionSettings.Scale
        |> Setting.bounded (0.5f, 5.0f)

    let explosion_expand_note = Setting.percentf data.NoteExplosionSettings.ExpandAmount

    let explosion_frame_time_hold =
        data.HoldExplosionSettings.AnimationFrameTime
        |> Setting.bounded (10.0f<ms / rate>, 1000.0f<ms / rate>)
        |> Setting.roundf_uom 0
        |> Setting.trigger (float >> f_hold_ex.set_Interval)

    let explosion_colors_hold = Setting.simple data.HoldExplosionSettings.Colors

    let explosion_offset_hold =
        data.HoldExplosionSettings.Offset
        |> Setting.bounded (-1.0f, 1.0f)

    let explosion_hold_use_release =
        Setting.simple data.HoldExplosionSettings.UseReleaseExplosion

    let explosion_builtin_release =
        Setting.simple data.HoldExplosionSettings.ReleaseUseBuiltInAnimation

    let explosion_duration_hold =
        data.HoldExplosionSettings.Duration
        |> Setting.bounded (50.0f<ms / rate>, 1000f<ms / rate>)
        |> Setting.roundf_uom 0
        |> Setting.trigger (float >> t_hold_ex.set_Interval)

    let explosion_scale_hold =
        data.HoldExplosionSettings.Scale
        |> Setting.bounded (0.5f, 5.0f)

    let explosion_expand_hold = Setting.percentf data.HoldExplosionSettings.ExpandAmount

    let note_explosions_subtab =
        NavigationContainer.Column(WrapNavigation = false)
        |+ PageSetting(
            %"noteskin.explosionanimationtime",
            Slider(Setting.uom explosion_frame_time_note, Step = 1f)
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
            Slider(Setting.uom explosion_duration_note, Step = 1f)
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
            Slider(Setting.uom explosion_frame_time_hold, Step = 1f)
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
            Slider(Setting.uom explosion_duration_hold, Step = 1f)
        )
            .Help(Help.Info("noteskin.explosionduration"))
            .Pos(12)
            .Conditional(fun () -> explosion_builtin_release.Value || not explosion_hold_use_release.Value)
        |+ PageSetting(%"noteskin.explosionexpand", Slider.Percent(explosion_expand_hold))
            .Help(Help.Info("noteskin.explosionexpand"))
            .Pos(14)
            .Conditional(fun () -> explosion_builtin_release.Value || not explosion_hold_use_release.Value)

    let explosions_tab =
        let subtab_container = SwapContainer(note_explosions_subtab)

        let subtab_options : (Widget * string) array =
            [|
                note_explosions_subtab, %"noteskin.note_explosions"
                hold_explosions_subtab, %"noteskin.hold_explosions"
            |]

        NavigationContainer.Column()
            .With(
                PageSetting(%"noteskin.enable_explosions", Checkbox enable_explosions).Pos(0),
                TabButtons.Create(subtab_options, subtab_container)
                    .Position(page_position(2, 2, PageWidth.Normal).TranslateY(20.0f).SliceT(TabButtons.HEIGHT))
                    .Conditional(enable_explosions.Get),
                subtab_container
                    .Position(Position.ShrinkT(150.0f))
                    .Conditional(enable_explosions.Get)
            )

    member this.SaveChanges() =
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

    override this.Content() =
        this.OnClose(this.SaveChanges)
        let tab_container = SwapContainer(receptors_tab)

        let tab_options : (Widget * string) array =
            [|
                receptors_tab, %"noteskin.receptors"
                judgement_line_tab, %"noteskin.judgement_line"
                column_light_tab, %"noteskin.column_lighting"
                explosions_tab, %"noteskin.explosions"
            |]

        NavigationContainer.Column()
            .With(
                TabButtons.Create(tab_options, tab_container)
                    .Position(page_position(0, 2, PageWidth.Normal).Translate(PAGE_MARGIN_X, PAGE_MARGIN_Y).SliceT(TabButtons.HEIGHT)),
                tab_container
                    .Position(Position.Shrink(PAGE_MARGIN_X, PAGE_MARGIN_Y).ShrinkT(100.0f))
            )

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

        let center = this.Bounds.Right - (this.Bounds.Width - PAGE_ITEM_WIDTH - PAGE_MARGIN_X * 2.0f) * 0.5f

        let mutable left = center - preview_width * COLUMN_WIDTH * 0.5f
        let position = this.Bounds.Top + this.Bounds.Height * 0.6f

        if enable_judgement_line.Value then
            Render.tex_quad
                (Rect
                    .FromSize(left, position - COLUMN_WIDTH, preview_width * COLUMN_WIDTH, COLUMN_WIDTH)
                    .ExpandPercentY(judgement_line_scale.Value * 0.5f - 0.5f)
                    .TranslateY(-COLUMN_WIDTH * judgement_line_offset.Value)
                    .AsQuad)
                Color.White.AsQuad
                (Sprite.pick_texture (f_note.Loops, 0) judgementline)

        let receptor() =
            if enable_receptors.Value then
                Render.tex_quad
                    (Rect
                        .FromSize(left, position - COLUMN_WIDTH, COLUMN_WIDTH, COLUMN_WIDTH / receptor.AspectRatio)
                        .TranslateY(-COLUMN_WIDTH * receptor_offset.Value)
                        .AsQuad)
                    Color.White.AsQuad
                    (Sprite.pick_texture (f_note.Loops, (if holding then 1 else 0)) receptor)

        if not notes_under_receptors.Value then
            receptor()

        Render.tex_quad
            (Rect
                .FromSize(left, position - COLUMN_WIDTH, COLUMN_WIDTH, COLUMN_WIDTH)
                .AsQuad)
            Color.White.AsQuad
            (Sprite.pick_texture (f_note.Loops, 0) note)

        if notes_under_receptors.Value then
            receptor()

        if enable_column_light.Value then
            left <- left + COLUMN_WIDTH
            receptor()

            let percent_remaining =
                if holding then
                    1.0f
                else
                    1.0 - t_columnlight.Progress
                    |> min 1.0
                    |> max 0.0
                    |> float32

            let a = 255.0f * percent_remaining |> int |> min 255 |> max 0

            Render.sprite
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
                    1.0 - (t_note_ex.Time / float explosion_duration_note.Value)
                    |> min 1.0
                    |> max 0.0
                    |> float32
                else if f_note_ex.Loops > noteexplosion.Columns then
                    0.0f
                else
                    1.0f

            let a = 255.0f * percent_remaining |> int

            Render.tex_quad
                (Rect
                    .FromSize(left, position - COLUMN_WIDTH, COLUMN_WIDTH, COLUMN_WIDTH)
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
                        1.0 - (t_hold_ex.Time / float explosion_duration_hold.Value)
                        |> min 1.0
                        |> max 0.0
                        |> float32

                let a = 255.0f * percent_remaining |> int

                Render.tex_quad
                    (Rect
                        .FromSize(left, position - COLUMN_WIDTH, COLUMN_WIDTH, COLUMN_WIDTH)
                        .Expand((explosion_scale_hold.Value - 1.0f) * COLUMN_WIDTH * 0.5f)
                        .Expand(explosion_expand_hold.Value * (1.0f - percent_remaining) * COLUMN_WIDTH)
                        .Translate(0.0f, -COLUMN_WIDTH * explosion_offset_hold.Value)
                        .AsQuad)
                    (Color.White.O4a a).AsQuad
                    (Sprite.pick_texture (f_hold_ex.Loops, 0) holdexplosion)

            else

                let percent_remaining =
                    if explosion_builtin_release.Value then
                        1.0 - (t_hold_ex.Time / float explosion_duration_hold.Value)
                        |> min 1.0
                        |> max 0.0
                        |> float32
                    else if f_hold_ex.Loops > releaseexplosion.Columns then
                        0.0f
                    else
                        1.0f

                let a = 255.0f * percent_remaining |> int

                Render.tex_quad
                    (Rect
                        .FromSize(left, position - COLUMN_WIDTH, COLUMN_WIDTH, COLUMN_WIDTH)
                        .Expand((explosion_scale_hold.Value - 1.0f) * COLUMN_WIDTH * 0.5f)
                        .Expand(explosion_expand_hold.Value * (1.0f - percent_remaining) * COLUMN_WIDTH)
                        .Translate(0.0f, -COLUMN_WIDTH * explosion_offset_hold.Value)
                        .AsQuad)
                    (Color.White.O4a a).AsQuad
                    (Sprite.pick_texture (f_hold_ex.Loops, 0) releaseexplosion)

    override this.Title = %"noteskin.animations"