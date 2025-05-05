namespace Interlude.Features.Skins.EditHUD

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Gameplay.Rulesets
open Prelude.Skins.HudLayouts
open Interlude.Content
open Interlude.UI

type JudgementDisplayPicker(ruleset: Ruleset, i: int, data: JudgementDisplayType array) =
    inherit Container(NodeType.Leaf)

    let texture = Content.Texture "judgements"

    let fd () =
        data.[i] <-
            match data.[i] with
            | JudgementDisplayType.Name ->
                JudgementDisplayType.Texture 0
            | JudgementDisplayType.Texture i ->
                if i + 1 >= texture.Rows then
                    JudgementDisplayType.Name
                else JudgementDisplayType.Texture (i + 1)
        Style.click.Play()

    let bk () =
        data.[i] <-
            match data.[i] with
            | JudgementDisplayType.Name ->
                JudgementDisplayType.Texture (texture.Rows - 1)
            | JudgementDisplayType.Texture 0 ->
                JudgementDisplayType.Name
            | JudgementDisplayType.Texture i ->
                JudgementDisplayType.Texture (i - 1)
        Style.click.Play()

    override this.Init(parent: Widget) =
        this
        |* MouseListener()
            .SelectOnClick(this, fd)
            .FocusOnHover(this)

        base.Init parent

    override this.Draw() =

        if this.Focused then
            Render.rect this.Bounds Colors.yellow_accent.O2
            if this.Selected then
                Render.rect (this.Bounds.SlicePercentR(0.5f).SliceB(5.0f).Shrink(100.0f, 0.0f)) Colors.yellow_accent

        Text.fill(Style.font, ruleset.JudgementName i, this.Bounds.SlicePercentL(0.5f).Shrink(10.0f, 5.0f), ruleset.JudgementColor i, Alignment.CENTER)
        Text.fill_b(Style.font, Icons.ARROW_RIGHT, this.Bounds.Shrink(10.0f, 5.0f), Colors.text_greyout, Alignment.CENTER)

        match data.[i] with
        | JudgementDisplayType.Name ->
            Text.fill(Style.font, ruleset.JudgementName i, this.Bounds.SlicePercentR(0.5f).Shrink(10.0f, 5.0f), ruleset.JudgementColor i, Alignment.CENTER)
        | JudgementDisplayType.Texture j ->
            Render.tex_quad (Sprite.fill (this.Bounds.SlicePercentR(0.5f).Shrink(10.0f, 5.0f)) texture).AsQuad Color.White.AsQuad (Sprite.pick_texture (0, j) texture)

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

type JudgementPage() =
    inherit Page()

    let config = Content.HUD

    let ignore_perfect_judgements =
        Setting.simple config.JudgementMeterIgnorePerfect

    let prioritise_lower_judgements =
        Setting.simple config.JudgementMeterPrioritiseLower

    let duration =
        config.JudgementMeterDuration
        |> Setting.bounded (100.0f<ms / rate>, 2000.0f<ms / rate>)

    let frame_time =
        config.JudgementMeterFrameTime
        |> Setting.bounded (2.0f<ms / rate>, 500.0f<ms / rate>)

    let use_animation =
        Setting.simple config.JudgementMeterUseBuiltInAnimation

    let use_texture =
        Setting.simple config.JudgementMeterUseTexture

    let texture = Content.Texture "judgements"
    let ruleset = Rulesets.current
    let JUDGEMENT_COUNT = ruleset.Judgements.Length
    let judgement_display =
        match config.JudgementMeterCustomDisplay.TryFind JUDGEMENT_COUNT with
        | Some existing -> Array.copy existing
        | None ->
            if texture.Rows = JUDGEMENT_COUNT then
                Array.init JUDGEMENT_COUNT JudgementDisplayType.Texture
            else
                Array.create JUDGEMENT_COUNT JudgementDisplayType.Name

    member this.SaveChanges() =
        Skins.save_hud_config
            { Content.HUD with
                JudgementMeterIgnorePerfect = ignore_perfect_judgements.Value
                JudgementMeterPrioritiseLower = prioritise_lower_judgements.Value
                JudgementMeterDuration = duration.Value
                JudgementMeterFrameTime = frame_time.Value
                JudgementMeterUseTexture = use_texture.Value
                JudgementMeterUseBuiltInAnimation = use_animation.Value
                JudgementMeterCustomDisplay = Content.HUD.JudgementMeterCustomDisplay.Add (JUDGEMENT_COUNT, judgement_display)
            }

    override this.Content() =
        this.OnClose(this.SaveChanges)

        page_container()
            .With(
                PageSetting(
                    %"hud.judgement.ignoreperfectjudgements",
                    Checkbox ignore_perfect_judgements
                )
                    .Help(Help.Info("hud.judgement.ignoreperfectjudgements"))
                    .Pos(0),
                PageSetting(
                    %"hud.judgement.prioritiselowerjudgements",
                    Checkbox prioritise_lower_judgements
                )
                    .Help(Help.Info("hud.judgement.prioritiselowerjudgements"))
                    .Pos(2),
                PageSetting(%"hud.judgement.duration", Slider(Setting.uom duration, Step = 5f))
                    .Help(Help.Info("hud.judgement.duration"))
                    .Pos(4)
                    .Conditional(fun () -> not use_texture.Value || use_animation.Value),
                PageSetting(%"hud.judgement.usetexture", Checkbox use_texture)
                    .Help(Help.Info("hud.judgement.usetexture"))
                    .Pos(6)
            )
            .WithConditional(
                use_texture.Get,

                PageSetting(
                    %"hud.judgement.useanimation",
                    Checkbox use_animation
                )
                    .Help(Help.Info("hud.judgement.useanimation"))
                    .Pos(8),
                PageSetting(%"hud.judgement.frametime", Slider(Setting.uom frame_time, Step = 5f))
                    .Help(Help.Info("hud.judgement.frametime"))
                    .Pos(10),
                ScrollContainer(
                    FlowContainer.Vertical<JudgementDisplayPicker>(PAGE_ITEM_HEIGHT)
                        .With(seq {
                            for i = 0 to ruleset.Judgements.Length - 1 do
                                yield JudgementDisplayPicker(ruleset, i, judgement_display)
                        })
                )
                    .Position(page_position(12, PAGE_BOTTOM - 10, PageWidth.Normal))
            )

    override this.Title = %"hud.judgement"