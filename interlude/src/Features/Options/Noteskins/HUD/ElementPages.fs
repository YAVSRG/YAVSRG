namespace Interlude.Features.OptionsMenu.Noteskins

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Prelude.Common
open Prelude.Content.Noteskins
open Interlude.Content
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Options
open Interlude.Features
open Interlude.Features.OptionsMenu.Gameplay

[<AbstractClass>]
type ConfigPreview(scale: float32, config: Setting<HUDPosition>) =
    inherit NoteskinPreview(scale, true)

    let keycount = int (Gameplay.Chart.keymode ())

    override this.Draw() =
        base.Draw()

        let container =
            if config.Value.RelativeToPlayfield then
                let cfg = Content.NoteskinConfig

                let width =
                    (cfg.ColumnWidth * float32 keycount
                     + Array.sum (cfg.KeymodeColumnSpacing keycount))
                    * scale

                let (screen_align, playfield_align) = cfg.PlayfieldAlignment

                Rect
                    .Box(
                        this.PreviewBounds.Left + this.PreviewBounds.Width * screen_align,
                        this.PreviewBounds.Top,
                        width,
                        this.PreviewBounds.Height
                    )
                    .Translate(-width * playfield_align, 0.0f)
            else
                this.PreviewBounds

        let width = container.Width
        let height = container.Height

        let leftA = snd config.Value.Left * width + container.Left
        let rightA = snd config.Value.Right * width + container.Left
        let topA = snd config.Value.Top * height + container.Top
        let bottomA = snd config.Value.Bottom * height + container.Top

        let bounds =
            Rect.Create(
                leftA + fst config.Value.Left * scale,
                topA + fst config.Value.Top * scale,
                rightA + fst config.Value.Right * scale,
                bottomA + fst config.Value.Bottom * scale
            )

        // Draw container
        Draw.rect
            (Rect
                .Create(container.Left, container.Top, container.Left, container.Bottom)
                .Expand(2.0f, 0.0f))
            Color.Red

        Draw.rect
            (Rect
                .Create(container.Right, container.Top, container.Right, container.Bottom)
                .Expand(2.0f, 0.0f))
            Color.Red

        Draw.rect
            (Rect
                .Create(container.Left, container.Top, container.Right, container.Top)
                .Expand(0.0f, 2.0f))
            Color.Red

        Draw.rect
            (Rect
                .Create(container.Left, container.Bottom, container.Right, container.Bottom)
                .Expand(0.0f, 2.0f))
            Color.Red
        // Draw alignments
        Draw.rect (Rect.Create(leftA, container.Top, leftA, container.Bottom).Expand(2.0f, 0.0f)) Color.Orange
        Draw.rect (Rect.Create(rightA, container.Top, rightA, container.Bottom).Expand(2.0f, 0.0f)) Color.Orange
        Draw.rect (Rect.Create(container.Left, topA, container.Right, topA).Expand(0.0f, 2.0f)) Color.Orange

        Draw.rect
            (Rect
                .Create(container.Left, bottomA, container.Right, bottomA)
                .Expand(0.0f, 2.0f))
            Color.Orange
        // Draw bounds
        Draw.rect
            (Rect
                .Create(bounds.Left, bounds.Top, bounds.Left, bounds.Bottom)
                .Expand(2.0f, 0.0f))
            Color.Lime

        Draw.rect
            (Rect
                .Create(bounds.Right, bounds.Top, bounds.Right, bounds.Bottom)
                .Expand(2.0f, 0.0f))
            Color.Lime

        Draw.rect
            (Rect
                .Create(bounds.Left, bounds.Top, bounds.Right, bounds.Top)
                .Expand(0.0f, 2.0f))
            Color.Lime

        Draw.rect
            (Rect
                .Create(bounds.Left, bounds.Bottom, bounds.Right, bounds.Bottom)
                .Expand(0.0f, 2.0f))
            Color.Lime

        this.DrawComponent(bounds)

    abstract member DrawComponent: Rect -> unit

[<AutoOpen>]
module Shared =

    let private noteskin_required =
        Callout
            .Small
            .Title(%"hud.noteskin_required.title")
            .Body(%"hud.noteskin_required.element")
            .Button(%"hud.noteskin_required.button", fun () -> Menu.Back(); Menu.Back(); NoteskinsPage().Show())

    let or_require_noteskin (items: Widget list) : Widget list =
        if Content.Noteskin.IsEmbedded then
            [ Callout.frame noteskin_required (fun (w, h) -> Position.Box(0.0f, 1.0f, 100.0f, -200.0f - h - 40.0f, w, h + 40.0f)) ]
        else items

type AccuracyPage(on_close: unit -> unit) as this =
    inherit Page()

    let user_options = options.HUD.Value
    let noteskin_options = Content.NoteskinConfig.HUD

    let pos = Setting.simple noteskin_options.AccuracyPosition

    let grade_colors = Setting.simple user_options.AccuracyGradeColors
    let show_name = Setting.simple user_options.AccuracyShowName

    let preview =
        { new ConfigPreview(0.35f, pos) with
            override this.DrawComponent(bounds) =
                Text.fill (Style.font, "96.72%", bounds.TrimBottom(bounds.Height * 0.3f), Color.White, 0.5f)

                if show_name.Value then
                    Text.fill (Style.font, "SC J4", bounds.SliceBottom(bounds.Height * 0.4f), Color.White, 0.5f)
        }

    do
        this.Content(
            column ()
            |+ PageSetting("hud.accuracy.gradecolors", Selector<_>.FromBool grade_colors)
                .Pos(200.0f)
                .Tooltip(Tooltip.Info("hud.accuracy.gradecolors"))
            |+ PageSetting("hud.accuracy.showname", Selector<_>.FromBool show_name)
                .Pos(270.0f)
                .Tooltip(Tooltip.Info("hud.accuracy.showname"))
            |+ preview
        )

    override this.Title = %"hud.accuracy.name"
    override this.OnDestroy() = preview.Destroy()

    override this.OnClose() =
        options.HUD.Set
            { options.HUD.Value with
                AccuracyGradeColors = grade_colors.Value
                AccuracyShowName = show_name.Value
            }

        Noteskins.save_hud_config
            { Content.NoteskinConfig.HUD with
                AccuracyPosition = pos.Value
            }

        on_close ()

type TimingDisplayPage(on_close: unit -> unit) as this =
    inherit Page()

    let user_options = options.HUD.Value
    let noteskin_options = Content.NoteskinConfig.HUD

    let pos = Setting.simple noteskin_options.TimingDisplayPosition

    let show_guide = Setting.simple user_options.TimingDisplayShowGuide
    let show_non_judgements = Setting.simple user_options.TimingDisplayShowNonJudgements

    let thickness =
        Setting.simple user_options.TimingDisplayThickness |> Setting.bound 1.0f 25.0f

    let release_thickness =
        Setting.simple user_options.TimingDisplayReleasesExtraHeight
        |> Setting.bound 0.0f 20.0f

    let half_scale_releases = Setting.simple user_options.TimingDisplayHalfScaleReleases

    let animation_time =
        Setting.simple user_options.TimingDisplayFadeTime
        |> Setting.bound 100.0f 2000.0f

    let preview =
        { new ConfigPreview(0.35f, pos) with
            override this.DrawComponent(bounds) =
                Draw.rect
                    (Rect.Create(
                        bounds.CenterX - thickness.Value / 2.0f,
                        bounds.Top,
                        bounds.CenterX + thickness.Value / 2.0f,
                        bounds.Bottom
                    ))
                    Color.White
        }

    do
        this.Content(
            column ()
            |+ PageSetting("hud.timingdisplay.showguide", Selector<_>.FromBool show_guide)
                .Pos(200.0f)
                .Tooltip(Tooltip.Info("hud.timingdisplay.showguide"))
            |+ PageSetting("hud.timingdisplay.shownonjudgements", Selector<_>.FromBool show_non_judgements)
                .Pos(270.0f)
                .Tooltip(Tooltip.Info("hud.timingdisplay.shownonjudgements"))
            |+ PageSetting("hud.timingdisplay.halfscalereleases", Selector<_>.FromBool half_scale_releases)
                .Pos(340.0f)
                .Tooltip(Tooltip.Info("hud.timingdisplay.halfscalereleases"))
            |+ PageSetting("hud.timingdisplay.thickness", Slider(thickness, Step = 1f))
                .Pos(410.0f)
                .Tooltip(Tooltip.Info("hud.timingdisplay.thickness"))
            |+ PageSetting("hud.timingdisplay.releasesextraheight", Slider(release_thickness, Step = 1f))
                .Pos(480.0f)
                .Tooltip(Tooltip.Info("hud.timingdisplay.releasesextraheight"))
            |+ PageSetting("hud.timingdisplay.animationtime", Slider(animation_time, Step = 5f))
                .Pos(550.0f)
                .Tooltip(Tooltip.Info("hud.timingdisplay.animationtime"))
            |+ preview
        )

    override this.Title = %"hud.timingdisplay.name"
    override this.OnDestroy() = preview.Destroy()

    override this.OnClose() =
        options.HUD.Set
            { options.HUD.Value with
                TimingDisplayShowGuide = show_guide.Value
                TimingDisplayShowNonJudgements = show_non_judgements.Value
                TimingDisplayThickness = thickness.Value
                TimingDisplayReleasesExtraHeight = release_thickness.Value
                TimingDisplayFadeTime = animation_time.Value
            }

        on_close ()

type ComboPage(on_close: unit -> unit) as this =
    inherit Page()

    let user_options = options.HUD.Value
    let noteskin_options = Content.NoteskinConfig.HUD

    let pos = Setting.simple noteskin_options.ComboPosition

    let lamp_colors = Setting.simple user_options.ComboLampColors

    let pop_amount =
        Setting.simple noteskin_options.ComboPop |> Setting.bound 0.0f 20.0f

    let growth_amount =
        Setting.simple noteskin_options.ComboGrowth |> Setting.bound 0.0f 0.05f

    let preview =
        { new ConfigPreview(0.35f, pos) with
            override this.DrawComponent(bounds) =
                Text.fill (Style.font, "727", bounds, Color.White, Alignment.CENTER)
        }

    do
        this.Content(
            column ()
            |+ PageSetting("hud.combo.lampcolors", Selector<_>.FromBool lamp_colors)
                .Pos(200.0f)
                .Tooltip(Tooltip.Info("hud.combo.lampcolors"))
            |+ ([
                PageSetting("hud.combo.pop", Slider(pop_amount, Step = 1f))
                    .Pos(270.0f)
                    .Tooltip(Tooltip.Info("hud.combo.pop")) :> Widget
                PageSetting("hud.combo.growth", Slider(growth_amount))
                    .Pos(340.0f)
                    .Tooltip(Tooltip.Info("hud.combo.growth"))
            ] |> or_require_noteskin)
            |+ preview
        )

    override this.Title = %"hud.combo.name"
    override this.OnDestroy() = preview.Destroy()

    override this.OnClose() =
        options.HUD.Set
            { options.HUD.Value with
                ComboLampColors = lamp_colors.Value
            }

        Noteskins.save_hud_config
            { Content.NoteskinConfig.HUD with
                ComboPop = pop_amount.Value
                ComboGrowth = growth_amount.Value
            }

        on_close ()

// currently unused
type SkipButtonPage(on_close: unit -> unit) as this =
    inherit Page()

    let user_options = options.HUD.Value
    let noteskin_options = Content.NoteskinConfig.HUD

    let pos = Setting.simple noteskin_options.SkipButtonPosition

    let preview_text = [ (%%"skip").ToString() ] %> "play.skiphint"

    let preview =
        { new ConfigPreview(0.35f, pos) with
            override this.DrawComponent(bounds) =
                Text.fill_b (Style.font, preview_text, bounds, Colors.text, Alignment.CENTER)
        }

    do this.Content(preview)

    override this.Title = %"hud.skipbutton.name"
    override this.OnDestroy() = preview.Destroy()

    override this.OnClose() =
        on_close ()

type ProgressMeterPage(on_close: unit -> unit) as this =
    inherit Page()

    let user_options = options.HUD.Value
    let noteskin_options = Content.NoteskinConfig.HUD

    let pos = Setting.simple noteskin_options.ProgressMeterPosition

    let color = Setting.simple noteskin_options.ProgressMeterColor
    let background_color = Setting.simple noteskin_options.ProgressMeterBackgroundColor
    let label = Setting.simple user_options.ProgressMeterLabel

    let preview =
        { new ConfigPreview(0.35f, pos) with
            override this.DrawComponent(bounds) =
                let x, y = bounds.Center
                let r = (min bounds.Width bounds.Height) * 0.5f
                let angle = System.MathF.PI / 15.0f

                let outer i =
                    let angle = float32 i * angle
                    let struct (a, b) = System.MathF.SinCos(angle)
                    (x + r * a, y - r * b)

                let inner i =
                    let angle = float32 i * angle
                    let struct (a, b) = System.MathF.SinCos(angle)
                    (x + (r - 2f) * a, y - (r - 2f) * b)

                for i = 0 to 29 do
                    Draw.untextured_quad
                        (Quad.createv (x, y) (x, y) (inner i) (inner (i + 1)))
                        (Quad.color background_color.Value)

                    Draw.untextured_quad
                        (Quad.createv (inner i) (outer i) (outer (i + 1)) (inner (i + 1)))
                        (Quad.color Colors.white.O2)

                for i = 0 to 17 do
                    Draw.untextured_quad (Quad.createv (x, y) (x, y) (inner i) (inner (i + 1))) (Quad.color color.Value)

                let text =
                    match label.Value with
                    | ProgressMeterLabel.Countdown -> "7:27"
                    | ProgressMeterLabel.Percentage -> "60%"
                    | _ -> ""

                Text.fill_b (
                    Style.font,
                    text,
                    bounds.BorderBottom(40.0f * 0.35f),
                    Colors.text_subheading,
                    Alignment.CENTER
                )
        }

    do
        this.Content(
            column ()
            |+ PageSetting("hud.progressmeter.label", Selector<ProgressMeterLabel>.FromEnum(label))
                .Pos(200.0f)
            |+ ([
                PageSetting("hud.progressmeter.color", ColorPicker(color, true))
                    .Pos(270.0f, PRETTYWIDTH, PRETTYHEIGHT * 1.5f) :> Widget
                PageSetting("hud.progressmeter.backgroundcolor", ColorPicker(background_color, true))
                    .Pos(375.0f, PRETTYWIDTH, PRETTYHEIGHT * 1.5f)
                ] |> or_require_noteskin)
            |+ preview
        )

    override this.Title = %"hud.progressmeter.name"
    override this.OnDestroy() = preview.Destroy()

    override this.OnClose() =
        options.HUD.Set
            { options.HUD.Value with
                ProgressMeterLabel = label.Value
            }

        Noteskins.save_hud_config
            { Content.NoteskinConfig.HUD with
                ProgressMeterColor = color.Value
                ProgressMeterBackgroundColor = background_color.Value
            }

        on_close ()

// currently unused
type PacemakerPage(on_close: unit -> unit) as this =
    inherit Page()

    let user_options = options.HUD.Value
    let noteskin_options = Content.NoteskinConfig.HUD

    let pos = Setting.simple noteskin_options.PacemakerPosition

    let preview =
        { new ConfigPreview(0.35f, pos) with
            override this.DrawComponent(bounds) =
                Text.fill_b (Style.font, Icons.FLAG, bounds, Colors.text, Alignment.CENTER)
        }

    do this.Content(preview)

    override this.Title = %"hud.pacemaker.name"
    override this.OnDestroy() = preview.Destroy()

    override this.OnClose() =
        on_close ()

type JudgementCounterPage(on_close: unit -> unit) as this =
    inherit Page()

    let user_options = options.HUD.Value
    let noteskin_options = Content.NoteskinConfig.HUD

    let pos = Setting.simple noteskin_options.JudgementCounterPosition

    let animation_time =
        Setting.simple user_options.JudgementCounterFadeTime
        |> Setting.bound 100.0 1000.0

    let preview =
        { new ConfigPreview(0.35f, pos) with
            override this.DrawComponent(bounds) =
                Draw.rect bounds (Color.FromArgb(127, 255, 255, 255))
        }

    do
        this.Content(
            column ()
            |+ PageSetting("hud.judgementcounter.animationtime", Slider(animation_time |> Setting.f32, Step = 5f))
                .Pos(200.0f)
                .Tooltip(Tooltip.Info("hud.judgementcounter.animationtime"))
            |+ preview
        )

    override this.Title = %"hud.judgementcounter.name"
    override this.OnDestroy() = preview.Destroy()

    override this.OnClose() =
        options.HUD.Set
            { options.HUD.Value with
                JudgementCounterFadeTime = animation_time.Value
            }

        on_close ()

type JudgementMeterPage(on_close: unit -> unit) as this =
    inherit Page()

    let user_options = options.HUD.Value
    let noteskin_options = Content.NoteskinConfig.HUD

    let pos = Setting.simple noteskin_options.JudgementMeterPosition

    let ignore_perfect_judgements =
        Setting.simple user_options.JudgementMeterIgnorePerfect

    let prioritise_lower_judgements =
        Setting.simple user_options.JudgementMeterPrioritiseLower

    let animation_time =
        Setting.simple user_options.JudgementMeterFadeTime
        |> Setting.bound 100.0f 2000.0f

    let use_texture =
        Setting.simple noteskin_options.JudgementMeterUseTexture

    let texture = Content.Texture "judgements"
    let rs = Rulesets.current
    let JUDGEMENT_COUNT = rs.Judgements.Length
    let judgement_display = 
        match noteskin_options.JudgementMeterCustomDisplay.TryFind JUDGEMENT_COUNT with
        | Some existing -> Array.copy existing
        | None -> 
            if texture.Rows = JUDGEMENT_COUNT then
                Array.init JUDGEMENT_COUNT JudgementDisplayType.Texture
            else
                Array.create JUDGEMENT_COUNT JudgementDisplayType.Name

    let preview =
        { new ConfigPreview(0.35f, pos) with
            override this.DrawComponent(bounds) =
                Text.fill (Style.font, rs.JudgementName 0, bounds, rs.JudgementColor 0, Alignment.CENTER)
        }

    do
        this.Content(
            column ()
            |+ PageSetting("hud.judgementmeter.animationtime", Slider(animation_time, Step = 5f))
                .Pos(200.0f)
                .Tooltip(Tooltip.Info("hud.judgementmeter.animationtime"))
            |+ PageSetting(
                "hud.judgementmeter.ignoreperfectjudgements",
                Selector<_>.FromBool(ignore_perfect_judgements)
            )
                .Pos(270.0f)
                .Tooltip(Tooltip.Info("hud.judgementmeter.ignoreperfectjudgements"))
            |+ PageSetting(
                "hud.judgementmeter.prioritiselowerjudgements",
                Selector<_>.FromBool(prioritise_lower_judgements)
            )
                .Pos(340.0f)
                .Tooltip(Tooltip.Info("hud.judgementmeter.prioritiselowerjudgements"))
            |+ PageSetting(
                "hud.judgementmeter.usetexture",
                Selector<_>.FromBool(use_texture)
            )
                .Pos(410.0f)
                .Tooltip(Tooltip.Info("hud.judgementmeter.usetexture"))
            |+ preview
        )

    override this.Title = %"hud.judgementmeter.name"
    override this.OnDestroy() = preview.Destroy()

    override this.OnClose() =
        options.HUD.Set
            { options.HUD.Value with
                JudgementMeterFadeTime = animation_time.Value
                JudgementMeterIgnorePerfect = ignore_perfect_judgements.Value
                JudgementMeterPrioritiseLower = prioritise_lower_judgements.Value
            }

        Noteskins.save_hud_config 
            { Content.NoteskinConfig.HUD with
                JudgementMeterUseTexture = use_texture.Value
                JudgementMeterCustomDisplay = Content.NoteskinConfig.HUD.JudgementMeterCustomDisplay.Add (JUDGEMENT_COUNT, judgement_display)
            }

        on_close ()

type EarlyLateMeterPage(on_close: unit -> unit) as this =
    inherit Page()

    let user_options = options.HUD.Value
    let noteskin_options = Content.NoteskinConfig.HUD

    let pos = Setting.simple noteskin_options.EarlyLateMeterPosition

    let animation_time =
        Setting.simple user_options.EarlyLateMeterFadeTime
        |> Setting.bound 100.0f 2000.0f

    let early_text = Setting.simple noteskin_options.EarlyLateMeterEarlyText
    let late_text = Setting.simple noteskin_options.EarlyLateMeterLateText
    let early_color = Setting.simple noteskin_options.EarlyLateMeterEarlyColor
    let late_color = Setting.simple noteskin_options.EarlyLateMeterLateColor

    let preview =
        { new ConfigPreview(0.35f, pos) with
            override this.DrawComponent(bounds) =
                Text.fill (Style.font, early_text.Value, bounds, early_color.Value, Alignment.CENTER)
        }

    do
        this.Content(
            column ()
            |+ PageSetting("hud.earlylatemeter.animationtime", Slider(animation_time, Step = 5f))
                .Pos(200.0f)
                .Tooltip(Tooltip.Info("hud.earlylatemeter.animationtime"))
            |+ ([
                PageTextEntry("hud.earlylatemeter.earlytext", early_text)
                    .Pos(270.0f)
                    .Tooltip(Tooltip.Info("hud.earlylatemeter.earlytext")) :> Widget
                PageSetting("hud.earlylatemeter.earlycolor", ColorPicker(early_color, false))
                    .Pos(340.0f, PRETTYWIDTH, PRETTYHEIGHT * 1.5f)
                    .Tooltip(Tooltip.Info("hud.earlylatemeter.earlycolor"))
                PageTextEntry("hud.earlylatemeter.latetext", late_text)
                    .Pos(445.0f)
                    .Tooltip(Tooltip.Info("hud.earlylatemeter.latetext"))
                PageSetting("hud.earlylatemeter.latecolor", ColorPicker(late_color, false))
                    .Pos(515.0f, PRETTYWIDTH, PRETTYHEIGHT * 1.5f)
                    .Tooltip(Tooltip.Info("hud.earlylatemeter.latecolor"))
                ] |> or_require_noteskin)
            |+ preview
        )

    override this.Title = %"hud.earlylatemeter.name"
    override this.OnDestroy() = preview.Destroy()

    override this.OnClose() =
        options.HUD.Set
            { options.HUD.Value with
                EarlyLateMeterFadeTime = animation_time.Value
            }

        Noteskins.save_hud_config
            { Content.NoteskinConfig.HUD with
                EarlyLateMeterPosition = pos.Value
                EarlyLateMeterEarlyText = early_text.Value
                EarlyLateMeterEarlyColor = early_color.Value
                EarlyLateMeterLateText = late_text.Value
                EarlyLateMeterLateColor = late_color.Value
            }

        on_close ()

type RateModMeterPage(on_close: unit -> unit) as this =
    inherit Page()

    let user_options = options.HUD.Value
    let noteskin_options = Content.NoteskinConfig.HUD

    let pos = Setting.simple noteskin_options.RateModMeterPosition

    let show_mods = Setting.simple user_options.RateModMeterShowMods

    let preview =
        { new ConfigPreview(0.35f, pos) with
            override this.DrawComponent(bounds) =
                Text.fill_b (
                    Style.font,
                    (if show_mods.Value then "1.00x, Mirror" else "1.00x"),
                    bounds,
                    Colors.text_subheading,
                    Alignment.CENTER
                )
        }

    do
        this.Content(
            column ()
            |+ PageSetting("hud.ratemodmeter.showmods", Selector<_>.FromBool(show_mods))
                .Pos(200.0f)
                .Tooltip(Tooltip.Info("hud.ratemodmeter.showmods"))
            |+ preview
        )

    override this.Title = %"hud.ratemodmeter.name"
    override this.OnDestroy() = preview.Destroy()

    override this.OnClose() =
        options.HUD.Set
            { options.HUD.Value with
                RateModMeterShowMods = show_mods.Value
            }

        on_close ()

// currently unused
type BPMMeterPage(on_close: unit -> unit) as this =
    inherit Page()

    let user_options = options.HUD.Value
    let noteskin_options = Content.NoteskinConfig.HUD

    let pos = Setting.simple noteskin_options.BPMMeterPosition

    let preview =
        { new ConfigPreview(0.35f, pos) with
            override this.DrawComponent(bounds) =
                Text.fill_b (Style.font, "727 BPM", bounds, Colors.text_subheading, Alignment.CENTER)
        }

    do this.Content(preview)

    override this.Title = %"hud.bpmmeter.name"
    override this.OnDestroy() = preview.Destroy()

    override this.OnClose() =
        on_close ()
