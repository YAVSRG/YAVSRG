namespace Interlude.Features.EditNoteskin

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Gameplay
open Prelude.Skinning.Noteskins
open Interlude.Content
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Options

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
        |* Clickable(
            (fun () ->
                this.Select true
                fd ()
            ),
            OnHover =
                fun b ->
                    if b && not this.Focused then
                        this.Focus true
                    elif not b && this.FocusedByMouse then
                        Selection.up true
        )

        base.Init parent

    override this.Draw() =

        if this.Focused then
            Draw.rect this.Bounds Colors.yellow_accent.O2
            if this.Selected then
                Draw.rect (this.Bounds.SliceRightPercent(0.5f).SliceBottom(5.0f).Shrink(100.0f, 0.0f)) Colors.yellow_accent

        Text.fill(Style.font, ruleset.JudgementName i, this.Bounds.SliceLeftPercent(0.5f).Shrink(10.0f, 5.0f), ruleset.JudgementColor i, Alignment.CENTER)
        Text.fill_b(Style.font, Icons.ARROW_RIGHT, this.Bounds.Shrink(10.0f, 5.0f), Colors.text_greyout, Alignment.CENTER)

        match data.[i] with
        | JudgementDisplayType.Name ->
            Text.fill(Style.font, ruleset.JudgementName i, this.Bounds.SliceRightPercent(0.5f).Shrink(10.0f, 5.0f), ruleset.JudgementColor i, Alignment.CENTER)
        | JudgementDisplayType.Texture j ->
            Draw.quad (Sprite.fill (this.Bounds.SliceRightPercent(0.5f).Shrink(10.0f, 5.0f)) texture).AsQuad Color.White.AsQuad (Sprite.pick_texture (0, j) texture)

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if this.Selected then
            if (%%"left").Tapped() then
                bk ()
            elif (%%"right").Tapped() then
                fd ()
            elif (%%"up").Tapped() then
                fd ()
            elif (%%"down").Tapped() then
                bk ()

type JudgementMeterPage(on_close: unit -> unit) =
    inherit Page()

    let user_options = options.HUD.Value
    let noteskin_options = Content.NoteskinConfig.HUD

    let ignore_perfect_judgements =
        Setting.simple user_options.JudgementMeterIgnorePerfect

    let prioritise_lower_judgements =
        Setting.simple user_options.JudgementMeterPrioritiseLower

    let duration =
        Setting.simple noteskin_options.JudgementMeterDuration
        |> Setting.bound 100.0f 2000.0f

    let frame_time =
        Setting.simple noteskin_options.JudgementMeterFrameTime
        |> Setting.bound 2.0f 500.0f

    let use_animation =
        Setting.simple noteskin_options.JudgementMeterUseBuiltInAnimation

    let use_texture =
        Setting.simple noteskin_options.JudgementMeterUseTexture

    let texture = Content.Texture "judgements"
    let ruleset = Rulesets.current
    let JUDGEMENT_COUNT = ruleset.Judgements.Length
    let judgement_display = 
        match noteskin_options.JudgementMeterCustomDisplay.TryFind JUDGEMENT_COUNT with
        | Some existing -> Array.copy existing
        | None -> 
            if texture.Rows = JUDGEMENT_COUNT then
                Array.init JUDGEMENT_COUNT JudgementDisplayType.Texture
            else
                Array.create JUDGEMENT_COUNT JudgementDisplayType.Name

    override this.Content() =
        page_container()
        |+ PageSetting(
            "hud.judgementmeter.ignoreperfectjudgements",
            Checkbox ignore_perfect_judgements
        )
            .Tooltip(Tooltip.Info("hud.judgementmeter.ignoreperfectjudgements"))
            .Pos(0)
        |+ PageSetting(
            "hud.judgementmeter.prioritiselowerjudgements",
            Checkbox prioritise_lower_judgements
        )
            .Tooltip(Tooltip.Info("hud.judgementmeter.prioritiselowerjudgements"))
            .Pos(2)
        |+ ([
            PageSetting("hud.judgementmeter.duration", Slider(duration, Step = 5f))
                .Tooltip(Tooltip.Info("hud.judgementmeter.duration"))
                .Pos(4)
                .Conditional(fun () -> not use_texture.Value || use_animation.Value)
            :> Widget
            PageSetting("hud.judgementmeter.usetexture", Checkbox use_texture)
                .Tooltip(Tooltip.Info("hud.judgementmeter.usetexture"))
                .Pos(6)
            PageSetting(
                "hud.judgementmeter.useanimation",
                Checkbox use_animation
            )
                .Tooltip(Tooltip.Info("hud.judgementmeter.useanimation"))
                .Pos(8)
                .Conditional(use_texture.Get)
            PageSetting("hud.judgementmeter.frametime", Slider(frame_time, Step = 5f))
                .Tooltip(Tooltip.Info("hud.judgementmeter.frametime"))
                .Pos(10)
                .Conditional(use_texture.Get)
            Conditional(use_texture.Get,
                FlowContainer.Vertical<Widget>(PRETTYHEIGHT)
                |+ seq {
                    for i = 0 to ruleset.Judgements.Length - 1 do
                        yield JudgementDisplayPicker(ruleset, i, judgement_display)
                }
                |> ScrollContainer,
                Position = pretty_pos(12, PAGE_BOTTOM - 10, PageWidth.Normal)
            )] |> or_require_noteskin)
        :> Widget

    override this.Title = %"hud.judgementmeter"

    override this.OnClose() =
        options.HUD.Set
            { options.HUD.Value with
                JudgementMeterIgnorePerfect = ignore_perfect_judgements.Value
                JudgementMeterPrioritiseLower = prioritise_lower_judgements.Value
            }

        Noteskins.save_hud_config 
            { Content.NoteskinConfig.HUD with
                JudgementMeterDuration = duration.Value
                JudgementMeterFrameTime = frame_time.Value
                JudgementMeterUseTexture = use_texture.Value
                JudgementMeterUseBuiltInAnimation = use_animation.Value
                JudgementMeterCustomDisplay = Content.NoteskinConfig.HUD.JudgementMeterCustomDisplay.Add (JUDGEMENT_COUNT, judgement_display)
            }

        on_close ()