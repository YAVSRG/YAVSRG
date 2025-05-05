namespace Interlude.Features.Skins.EditHUD

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Gameplay.Rulesets
open Interlude.Content
open Interlude.UI
open Interlude.Features.Play.HUD

type private DisplayPicker(ruleset: Ruleset, i: int, data: int option array) =
    inherit Container(NodeType.Leaf)

    let texture = Content.Texture "judgement-counter-judgements"

    let fd () =
        data.[i] <-
            match data.[i] with
            | None -> Some 0
            | Some i -> if i + 1 >= texture.Rows then None else Some(i + 1)

        Style.click.Play()

    let bk () =
        data.[i] <-
            match data.[i] with
            | None -> Some(texture.Rows - 1)
            | Some 0 -> None
            | Some i -> Some(i - 1)

        Style.click.Play()

    override this.Init(parent: Widget) =
        this
            .Add(
                MouseListener()
                    .SelectOnClick(this, fd)
                    .FocusOnHover(this)
            )

        base.Init parent

    override this.Draw() =

        if this.Focused then
            Render.rect this.Bounds Colors.yellow_accent.O2

            if this.Selected then
                Render.rect (this.Bounds.SlicePercentR(0.5f).SliceB(5.0f).Shrink(100.0f, 0.0f)) Colors.yellow_accent

        Text.fill (
            Style.font,
            ruleset.JudgementName i,
            this.Bounds.SlicePercentL(0.5f).Shrink(10.0f, 5.0f),
            ruleset.JudgementColor i,
            Alignment.CENTER
        )

        Text.fill_b (
            Style.font,
            Icons.ARROW_RIGHT,
            this.Bounds.Shrink(10.0f, 5.0f),
            Colors.text_greyout,
            Alignment.CENTER
        )

        match data.[i] with
        | None ->
            Text.fill_b (
                Style.font,
                ruleset.JudgementName i,
                this.Bounds.SlicePercentR(0.5f).Shrink(10.0f, 5.0f),
                (Color.White, Color.Black),
                Alignment.CENTER
            )
        | Some j ->
            Render.tex_quad
                (Sprite.fill (this.Bounds.SlicePercentR(0.5f).Shrink(10.0f, 5.0f)) texture)
                    .AsQuad
                Color.White.AsQuad
                (Sprite.pick_texture (0, j) texture)

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if this.Selected then
            if (%%"left").Pressed() then
                bk ()
            elif (%%"right").Pressed() then
                fd ()
            elif (%%"up").Pressed() then
                fd ()
            elif (%%"down").Pressed() then
                bk ()

type private JudgementCounterDisplayPage(use_texture: Setting<bool>, display: int option array, ruleset: Ruleset) =
    inherit Page()

    override this.Content() =
        page_container ()
        |+ PageSetting(%"hud.judgement_counter.show_labels", Checkbox use_texture)
            .Help(Help.Info("hud.judgement_counter.show_labels"))
            .Pos(0)
        |+ Conditional(
            use_texture.Get,
            FlowContainer.Vertical<Widget>(PAGE_ITEM_HEIGHT)
            |+ seq {
                for i = 0 to ruleset.Judgements.Length - 1 do
                    yield DisplayPicker(ruleset, i, display)
            }
            |> ScrollContainer).Position(page_position (2, PAGE_BOTTOM - 10, PageWidth.Normal))
        :> Widget

    override this.Title = %"hud.judgement_counter.textures"

type JudgementCounterPage() =
    inherit Page()

    let config = Content.HUD
    let texture = Content.Texture "judgement-counter-judgements"
    let font_texture = Content.Texture "judgement-counter-font"

    let pos = Setting.simple config.JudgementCounterPosition

    let animation_time =
        config.JudgementCounterFadeTime
        |> Setting.bounded (50.0f<ms / rate>, 1000.0f<ms / rate>)

    let pop_amount = config.JudgementCounterPopAmount |> Setting.bounded (-1.0f, 1.0f)

    let opacity = Setting.percentf config.JudgementCounterOpacity

    let text_scale = Setting.percentf config.JudgementCounterTextScale

    let use_font = Setting.simple config.JudgementCounterUseFont

    let font_spacing =
        config.JudgementCounterFontSpacing |> Setting.bounded (-1.0f, 1.0f)

    let font_dot_spacing =
        config.JudgementCounterDotExtraSpacing |> Setting.bounded (-1.0f, 1.0f)

    let font_colon_spacing =
        config.JudgementCounterColonExtraSpacing |> Setting.bounded (-1.0f, 1.0f)

    let show_ratio = Setting.simple config.JudgementCounterShowRatio

    let show_labels = Setting.simple config.JudgementCounterShowLabels
    let ruleset = Rulesets.current
    let JUDGEMENT_COUNT = ruleset.Judgements.Length

    let display: int option array =
        match config.JudgementCounterCustomDisplay.TryFind JUDGEMENT_COUNT with
        | Some existing -> Array.copy existing
        | None ->
            if texture.Rows = JUDGEMENT_COUNT then
                Array.init JUDGEMENT_COUNT Some
            else
                Array.create JUDGEMENT_COUNT None

    let preview =
        { new ElementPreview(pos.Value) with
            override this.DrawComponent(bounds) =
                let opacity = 255f * opacity.Value |> int |> max 0 |> min 255

                let count_alignment =
                    if show_labels.Value then
                        Alignment.RIGHT
                    else
                        Alignment.CENTER

                let h =
                    bounds.Height
                    / float32 (ruleset.Judgements.Length + if show_ratio.Value then 1 else 0)
                    |> min bounds.Width

                let mutable r = bounds.SliceT(h).Shrink(10.0f)

                for i = 0 to ruleset.Judgements.Length - 1 do
                    let j = ruleset.Judgements.[i]

                    Render.border 5.0f r (j.Color.O3a opacity)
                    Render.rect (r.BorderL(5.0f).SlicePercentY(0.45f)) j.Color.O2
                    Render.rect r (j.Color.O1a opacity)

                    if show_labels.Value then

                        match display.[i] with
                        | Some texture_index ->
                            Render.tex_quad
                                ((Sprite.fill_left (r.SlicePercentY(text_scale.Value).ShrinkX(10.0f)) texture).AsQuad)
                                Color.White.AsQuad
                                (Sprite.pick_texture (0, texture_index) texture)
                        | None ->
                            Text.fill_b (
                                Style.font,
                                j.Name,
                                r.SlicePercentY(text_scale.Value / 0.6f).ShrinkX(10.0f),
                                (Color.White, Color.Black),
                                Alignment.LEFT
                            )

                    if use_font.Value then
                        JudgementCounter.draw_count_aligned (
                            font_texture,
                            (
                                if show_labels.Value then
                                    r.SlicePercentY(text_scale.Value).ShrinkX(10.0f)
                                else
                                    r.SlicePercentY(text_scale.Value).ExpandPercentX(2.0f)
                            ),
                            Color.White,
                            730 - 7 * i,
                            font_spacing.Value,
                            count_alignment
                        )
                    else
                        Text.fill_b (
                            Style.font,
                            sprintf "%i" (730 - 7 * i),
                            (
                                if show_labels.Value then
                                    r.SlicePercentY(text_scale.Value / 0.6f).ShrinkX(10.0f)
                                else
                                    r.SlicePercentY(text_scale.Value / 0.6f).ExpandPercentX(2.0f)
                            ),
                            (Color.White, Color.Black),
                            count_alignment
                        )

                    r <- r.Translate(0.0f, h)

                if show_ratio.Value then
                    let ratio = 727, 100

                    if use_font.Value then
                        JudgementCounter.draw_ratio_centered (
                            font_texture,
                            r.SlicePercentT(text_scale.Value).ExpandPercentX(2.0f),
                            Color.White,
                            ratio,
                            font_spacing.Value,
                            font_dot_spacing.Value,
                            font_colon_spacing.Value
                        )
                    else
                        let (mv, pf) = ratio

                        Text.fill_b (
                            Style.font,
                            (if pf = 0 then
                                 sprintf "%.1f:0" (float mv)
                             else
                                 sprintf "%.1f:1" (float mv / float pf)),
                            r.SlicePercentT(text_scale.Value / 0.6f).ExpandPercentX(2.0f),
                            (Color.White, Color.Black),
                            Alignment.CENTER
                        )
        }

    member this.SaveChanges() =
        Skins.save_hud_config
            { Content.HUD with
                JudgementCounterFadeTime = animation_time.Value
                JudgementCounterPopAmount = pop_amount.Value
                JudgementCounterOpacity = opacity.Value

                JudgementCounterTextScale = text_scale.Value
                JudgementCounterUseFont = use_font.Value
                JudgementCounterFontSpacing = font_spacing.Value
                JudgementCounterDotExtraSpacing = font_dot_spacing.Value
                JudgementCounterColonExtraSpacing = font_colon_spacing.Value
                JudgementCounterShowRatio = show_ratio.Value

                JudgementCounterShowLabels = show_labels.Value
                JudgementCounterCustomDisplay = Content.HUD.JudgementCounterCustomDisplay.Add(JUDGEMENT_COUNT, display)
            }

    override this.Content() =
        this.OnClose(this.SaveChanges)

        page_container()
            .With(
                PageSetting(%"hud.judgement_counter.animationtime", Slider(Setting.uom animation_time, Step = 5f))
                    .Help(Help.Info("hud.judgement_counter.animationtime"))
                    .Pos(0),
                PageSetting(%"hud.judgement_counter.pop_amount", Slider.Percent(pop_amount))
                    .Help(Help.Info("hud.judgement_counter.pop_amount"))
                    .Pos(2),
                PageSetting(%"hud.judgement_counter.opacity", Slider.Percent(opacity))
                    .Help(Help.Info("hud.judgement_counter.opacity"))
                    .Pos(4),
                PageSetting(%"hud.judgement_counter.text_scale", Slider.Percent(text_scale))
                    .Help(Help.Info("hud.judgement_counter.text_scale"))
                    .Pos(6),
                PageSetting(%"hud.judgement_counter.showratio", Checkbox show_ratio)
                    .Help(Help.Info("hud.judgement_counter.showratio"))
                    .Pos(8),
                PageButton(
                    %"hud.judgement_counter.judgement_labels",
                    fun () -> JudgementCounterDisplayPage(show_labels, display, ruleset).Show()
                )
                    .Pos(11),
                PageSetting(%"hud.generic.use_font", Checkbox use_font)
                    .Help(Help.Info("hud.generic.use_font"))
                    .Pos(14)
            )
            .WithConditional(
                use_font.Get,

                PageSetting(%"hud.generic.font_spacing", Slider.Percent(font_spacing))
                    .Help(Help.Info("hud.generic.font_spacing"))
                    .Pos(16),
                PageSetting(%"hud.generic.dot_spacing", Slider.Percent(font_dot_spacing))
                    .Help(Help.Info("hud.generic.dot_spacing"))
                    .Pos(18),
                PageSetting(%"hud.generic.colon_spacing", Slider.Percent(font_colon_spacing))
                    .Help(Help.Info("hud.generic.colon_spacing"))
                    .Pos(20)
            )
        |> Container.Create
        |+ preview
        :> Widget

    override this.Title = %"hud.judgement_counter"