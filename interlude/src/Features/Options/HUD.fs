namespace Interlude.Features.OptionsMenu.HUD

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Prelude.Common
open Prelude.Data.Content
open Interlude.Content
open Interlude.Utils
open Interlude.UI
open Interlude.Options
open Interlude.UI.Menu
open Interlude.UI.Components
open Interlude.Features.OptionsMenu.Gameplay

[<AutoOpen>]
module private Helpers =

    [<AbstractClass>]
    type PositionEditor(icon: string) =
        inherit StaticContainer(NodeType.Leaf)

        let mutable repeat = -1
        let mutable time = 0.0
        let REPEAT_DELAY = 400.0
        let REPEAT_INTERVAL = 40.0

        override this.OnFocus() =
            Style.hover.Play()
            base.OnFocus()

        override this.Init(parent) =
            base.Init parent
            this |+ Text(icon, Align = Alignment.LEFT) |* Clickable.Focus this

        override this.Update(elapsed_ms, moved) =
            base.Update(elapsed_ms, moved)

            if this.Selected then
                let u = (%%"up").Tapped()
                let d = (%%"down").Tapped()
                let l = (%%"left").Tapped()
                let r = (%%"right").Tapped()

                if u || d || l || r then
                    repeat <- 0
                    time <- 0

                    if u then
                        this.Up()

                    if d then
                        this.Down()

                    if l then
                        this.Left()

                    if r then
                        this.Right()

                if repeat >= 0 then
                    let u = (%%"up").Pressed()
                    let d = (%%"down").Pressed()
                    let l = (%%"left").Pressed()
                    let r = (%%"right").Pressed()

                    time <- time + elapsed_ms

                    if (float repeat * REPEAT_INTERVAL + REPEAT_DELAY < time) then
                        repeat <- repeat + 1

                        if u then
                            this.Up()

                        if d then
                            this.Down()

                        if l then
                            this.Left()

                        if r then
                            this.Right()

                    if not (u || d || l || r) then
                        repeat <- -1

        abstract member Up: unit -> unit
        abstract member Down: unit -> unit
        abstract member Left: unit -> unit
        abstract member Right: unit -> unit

    let position_editor (setting: Setting<WidgetPosition>) (default_pos: WidgetPosition) =
        column ()
        |+ PageSetting(
            "hud.generic.enable",
            Selector<_>
                .FromBool(
                    Setting.make
                        (fun v -> setting.Set { setting.Value with Enabled = v })
                        (fun () -> setting.Value.Enabled)
                )
        )
            .Pos(100.0f)
        |+ PageSetting(
            "hud.generic.float",
            Selector<_>
                .FromBool(
                    Setting.make (fun v -> setting.Set { setting.Value with Float = v }) (fun () -> setting.Value.Float)
                )
        )
            .Pos(170.0f)
            .Tooltip(Tooltip.Info("hud.generic.float"))

        |+ PageSetting(
            "hud.generic.move",
            { new PositionEditor(Icons.move) with
                override this.Up() =
                    { setting.Value with
                        Top = setting.Value.Top - 5.0f
                        Bottom = setting.Value.Bottom - 5.0f
                    }
                    |> setting.Set

                override this.Down() =
                    { setting.Value with
                        Top = setting.Value.Top + 5.0f
                        Bottom = setting.Value.Bottom + 5.0f
                    }
                    |> setting.Set

                override this.Left() =
                    { setting.Value with
                        Left = setting.Value.Left - 5.0f
                        Right = setting.Value.Right - 5.0f
                    }
                    |> setting.Set

                override this.Right() =
                    { setting.Value with
                        Left = setting.Value.Left + 5.0f
                        Right = setting.Value.Right + 5.0f
                    }
                    |> setting.Set
            }
        )
            .Pos(240.0f)
            .Tooltip(Tooltip.Info("hud.generic.move"))

        |+ PageSetting(
            "hud.generic.grow",
            { new PositionEditor(Icons.grow) with
                override this.Up() =
                    { setting.Value with
                        Top = setting.Value.Top - 5.0f
                    }
                    |> setting.Set

                override this.Down() =
                    { setting.Value with
                        Bottom = setting.Value.Bottom + 5.0f
                    }
                    |> setting.Set

                override this.Left() =
                    { setting.Value with
                        Left = setting.Value.Left - 5.0f
                    }
                    |> setting.Set

                override this.Right() =
                    { setting.Value with
                        Right = setting.Value.Right + 5.0f
                    }
                    |> setting.Set
            }
        )
            .Pos(310.0f)
            .Tooltip(Tooltip.Info("hud.generic.grow"))

        |+ PageSetting(
            "hud.generic.shrink",
            { new PositionEditor(Icons.shrink) with
                override this.Up() =
                    { setting.Value with
                        Bottom = setting.Value.Bottom - 5.0f
                    }
                    |> setting.Set

                override this.Down() =
                    { setting.Value with
                        Top = setting.Value.Top + 5.0f
                    }
                    |> setting.Set

                override this.Left() =
                    { setting.Value with
                        Right = setting.Value.Right - 5.0f
                    }
                    |> setting.Set

                override this.Right() =
                    { setting.Value with
                        Left = setting.Value.Left + 5.0f
                    }
                    |> setting.Set
            }
        )
            .Pos(380.0f)
            .Tooltip(Tooltip.Info("hud.generic.shrink"))

        |+ PageButton(
            "hud.generic.reset",
            fun () ->
                setting.Value <-
                    { default_pos with
                        Enabled = setting.Value.Enabled
                    }
        )
            .Pos(450.0f)
            .Tooltip(Tooltip.Info("hud.generic.reset"))

type EditAccuracyMeterPage() as this =
    inherit Page()

    let data = HUDOptions.get<HUD.AccuracyMeter> ()

    let pos = Setting.simple data.Position
    let default_pos = HUD.AccuracyMeter.Default.Position

    let grade_colors = Setting.simple data.GradeColors
    let show_name = Setting.simple data.ShowName

    let preview =
        { new ConfigPreview(0.5f, pos) with
            override this.DrawComponent(bounds) =
                Text.fill (Style.font, "96.72%", bounds.TrimBottom(bounds.Height * 0.3f), Color.White, 0.5f)

                if show_name.Value then
                    Text.fill (Style.font, "SC+ J4", bounds.SliceBottom(bounds.Height * 0.4f), Color.White, 0.5f)
        }

    do
        this.Content(
            position_editor pos default_pos
            |+ PageSetting("hud.accuracymeter.gradecolors", Selector<_>.FromBool grade_colors)
                .Pos(550.0f)
                .Tooltip(Tooltip.Info("hud.accuracymeter.gradecolors"))
            |+ PageSetting("hud.accuracymeter.showname", Selector<_>.FromBool show_name)
                .Pos(620.0f)
                .Tooltip(Tooltip.Info("hud.accuracymeter.showname"))
            |+ preview
        )

    override this.Title = %"hud.accuracymeter.name"
    override this.OnDestroy() = preview.Destroy()

    override this.OnClose() =
        HUDOptions.set<HUD.AccuracyMeter>
            {
                Position = pos.Value
                GradeColors = grade_colors.Value
                ShowName = show_name.Value
            }

type EditHitMeterPage() as this =
    inherit Page()

    let data = HUDOptions.get<HUD.HitMeter> ()

    let pos = Setting.simple data.Position
    let default_pos = HUD.HitMeter.Default.Position

    let show_guide = Setting.simple data.ShowGuide
    let show_non_judgements = Setting.simple data.ShowNonJudgements
    let thickness = Setting.simple data.Thickness |> Setting.bound 1.0f 25.0f

    let release_thickness =
        Setting.simple data.ReleasesExtraHeight |> Setting.bound 0.0f 20.0f

    let half_scale_releases = Setting.simple data.HalfScaleReleases

    let animation_time =
        Setting.simple data.AnimationTime |> Setting.bound 100.0f 2000.0f

    let preview =
        { new ConfigPreview(0.5f, pos) with
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
            position_editor pos default_pos
            |+ PageSetting("hud.hitmeter.showguide", Selector<_>.FromBool show_guide)
                .Pos(550.0f)
                .Tooltip(Tooltip.Info("hud.hitmeter.showguide"))
            |+ PageSetting("hud.hitmeter.shownonjudgements", Selector<_>.FromBool show_non_judgements)
                .Pos(620.0f)
                .Tooltip(Tooltip.Info("hud.hitmeter.shownonjudgements"))
            |+ PageSetting("hud.hitmeter.halfscalereleases", Selector<_>.FromBool half_scale_releases)
                .Pos(690.0f)
                .Tooltip(Tooltip.Info("hud.hitmeter.halfscalereleases"))
            |+ PageSetting("hud.hitmeter.thickness", Slider(thickness, Step = 1f))
                .Pos(760.0f)
                .Tooltip(Tooltip.Info("hud.hitmeter.thickness"))
            |+ PageSetting("hud.hitmeter.releasesextraheight", Slider(release_thickness, Step = 1f))
                .Pos(830.0f)
                .Tooltip(Tooltip.Info("hud.hitmeter.releasesextraheight"))
            |+ PageSetting("hud.hitmeter.animationtime", Slider(animation_time, Step = 5f))
                .Pos(900.0f)
                .Tooltip(Tooltip.Info("hud.hitmeter.animationtime"))
            |+ preview
        )

    override this.Title = %"hud.hitmeter.name"
    override this.OnDestroy() = preview.Destroy()

    override this.OnClose() =
        HUDOptions.set<HUD.HitMeter>
            { data with
                Position = pos.Value
                ShowGuide = show_guide.Value
                ShowNonJudgements = show_non_judgements.Value
                Thickness = thickness.Value
                ReleasesExtraHeight = release_thickness.Value
                AnimationTime = animation_time.Value
            }

type EditComboMeterPage() as this =
    inherit Page()

    let data = HUDOptions.get<HUD.Combo> ()

    let pos = Setting.simple data.Position
    let default_pos = HUD.Combo.Default.Position

    let lamp_colors = Setting.simple data.LampColors
    let pop_amount = Setting.simple data.Pop |> Setting.bound 0.0f 20.0f
    let growth_amount = Setting.simple data.Growth |> Setting.bound 0.0f 0.05f

    let preview =
        { new ConfigPreview(0.5f, pos) with
            override this.DrawComponent(bounds) =
                Text.fill (Style.font, "727", bounds, Color.White, Alignment.CENTER)
        }

    do
        this.Content(
            position_editor pos default_pos
            |+ PageSetting("hud.combo.lampcolors", Selector<_>.FromBool lamp_colors)
                .Pos(550.0f)
                .Tooltip(Tooltip.Info("hud.combo.lampcolors"))
            |+ PageSetting("hud.combo.pop", Slider(pop_amount, Step = 1f))
                .Pos(620.0f)
                .Tooltip(Tooltip.Info("hud.combo.pop"))
            |+ PageSetting("hud.combo.growth", Slider(growth_amount))
                .Pos(690.0f)
                .Tooltip(Tooltip.Info("hud.combo.growth"))
            |+ preview
        )

    override this.Title = %"hud.combo.name"
    override this.OnDestroy() = preview.Destroy()

    override this.OnClose() =
        HUDOptions.set<HUD.Combo>
            {
                Position = pos.Value
                LampColors = lamp_colors.Value
                Pop = pop_amount.Value
                Growth = growth_amount.Value
            }

type EditSkipButtonPage() as this =
    inherit Page()

    let data = HUDOptions.get<HUD.SkipButton> ()

    let pos = Setting.simple data.Position
    let default_pos = HUD.SkipButton.Default.Position

    let preview_text = [ (%%"skip").ToString() ] %> "play.skiphint"

    let preview =
        { new ConfigPreview(0.5f, pos) with
            override this.DrawComponent(bounds) =
                Text.fill_b (Style.font, preview_text, bounds, Colors.text, Alignment.CENTER)
        }

    do this.Content(position_editor pos default_pos |+ preview)

    override this.Title = %"hud.skipbutton.name"
    override this.OnDestroy() = preview.Destroy()

    override this.OnClose() =
        HUDOptions.set<HUD.SkipButton> { Position = pos.Value }

type EditProgressMeterPage() as this =
    inherit Page()

    let data = HUDOptions.get<HUD.ProgressMeter> ()

    let pos = Setting.simple data.Position
    let default_pos = HUD.ProgressMeter.Default.Position

    let color = Setting.simple data.Color
    let background_color = Setting.simple data.BackgroundColor
    let label = Setting.simple data.Label

    let preview =
        { new ConfigPreview(0.5f, pos) with
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
                    Draw.quad
                        (Quad.createv (x, y) (x, y) (inner i) (inner (i + 1)))
                        (Quad.color background_color.Value)
                        Sprite.DEFAULT_QUAD

                    Draw.quad
                        (Quad.createv (inner i) (outer i) (outer (i + 1)) (inner (i + 1)))
                        (Quad.color Colors.white.O2)
                        Sprite.DEFAULT_QUAD

                for i = 0 to 17 do
                    Draw.quad
                        (Quad.createv (x, y) (x, y) (inner i) (inner (i + 1)))
                        (Quad.color color.Value)
                        Sprite.DEFAULT_QUAD

                let text =
                    match label.Value with
                    | HUD.ProgressMeterLabel.Countdown -> "7:27"
                    | HUD.ProgressMeterLabel.Percentage -> "60%"
                    | _ -> ""

                Text.fill_b (
                    Style.font,
                    text,
                    bounds.Expand(0.0f, 20.0f).SliceBottom(20.0f),
                    Colors.text_subheading,
                    Alignment.CENTER
                )
        }

    do
        this.Content(
            position_editor pos default_pos
            |+ PageSetting("hud.progressmeter.label", Selector<HUD.ProgressMeterLabel>.FromEnum(label))
                .Pos(550.0f)
            |+ PageSetting("hud.progressmeter.color", ColorPicker(color, true))
                .Pos(620.0f, PRETTYWIDTH, PRETTYHEIGHT * 1.5f)
            |+ PageSetting("hud.progressmeter.backgroundcolor", ColorPicker(background_color, true))
                .Pos(725.0f, PRETTYWIDTH, PRETTYHEIGHT * 1.5f)
            |+ preview
        )

    override this.Title = %"hud.progressmeter.name"
    override this.OnDestroy() = preview.Destroy()

    override this.OnClose() =
        HUDOptions.set<HUD.ProgressMeter>
            {
                Position = pos.Value
                Label = label.Value
                Color = color.Value
                BackgroundColor = background_color.Value
            }

type EditPacemakerPage() as this =
    inherit Page()

    let data = HUDOptions.get<HUD.Pacemaker> ()

    let pos = Setting.simple data.Position
    let default_pos = HUD.Pacemaker.Default.Position

    let preview =
        { new ConfigPreview(0.5f, pos) with
            override this.DrawComponent(bounds) =
                Text.fill_b (Style.font, Icons.goal, bounds, Colors.text, Alignment.CENTER)
        }

    do this.Content(position_editor pos default_pos |+ preview)

    override this.Title = %"hud.pacemaker.name"
    override this.OnDestroy() = preview.Destroy()

    override this.OnClose() =
        HUDOptions.set<HUD.Pacemaker> { Position = pos.Value }

type EditJudgementCountsPage() as this =
    inherit Page()

    let data = HUDOptions.get<HUD.JudgementCounts> ()

    let pos = Setting.simple data.Position
    let default_pos = HUD.JudgementCounts.Default.Position

    let animation_time = Setting.simple data.AnimationTime |> Setting.bound 100.0 1000.0

    let preview =
        { new ConfigPreview(0.5f, pos) with
            override this.DrawComponent(bounds) =
                Draw.rect bounds (Color.FromArgb(127, 255, 255, 255))
        }

    do
        this.Content(
            position_editor pos default_pos
            |+ PageSetting("hud.judgementcounts.animationtime", Slider(animation_time |> Setting.f32, Step = 5f))
                .Pos(550.0f)
                .Tooltip(Tooltip.Info("hud.judgementcounts.animationtime"))
            |+ preview
        )

    override this.Title = %"hud.judgementcounts.name"
    override this.OnDestroy() = preview.Destroy()

    override this.OnClose() =
        HUDOptions.set<HUD.JudgementCounts>
            {
                Position = pos.Value
                AnimationTime = animation_time.Value
            }

type EditJudgementMeterPage() as this =
    inherit Page()

    let data = HUDOptions.get<HUD.JudgementMeter> ()

    let pos = Setting.simple data.Position
    let default_pos = HUD.JudgementMeter.Default.Position

    let ignore_perfect_judgements = Setting.simple data.IgnorePerfectJudgements
    let prioritise_lower_judgements = Setting.simple data.PrioritiseLowerJudgements

    let animation_time =
        Setting.simple data.AnimationTime |> Setting.bound 100.0f 2000.0f

    let rs = Rulesets.current

    let preview =
        { new ConfigPreview(0.5f, pos) with
            override this.DrawComponent(bounds) =
                Text.fill (Style.font, rs.JudgementName 0, bounds, rs.JudgementColor 0, Alignment.CENTER)
        }

    do
        this.Content(
            position_editor pos default_pos
            |+ PageSetting("hud.judgementmeter.animationtime", Slider(animation_time, Step = 5f))
                .Pos(550.0f)
                .Tooltip(Tooltip.Info("hud.judgementmeter.animationtime"))
            |+ PageSetting(
                "hud.judgementmeter.ignoreperfectjudgements",
                Selector<_>.FromBool(ignore_perfect_judgements)
            )
                .Pos(620.0f)
                .Tooltip(Tooltip.Info("hud.judgementmeter.ignoreperfectjudgements"))
            |+ PageSetting(
                "hud.judgementmeter.prioritiselowerjudgements",
                Selector<_>.FromBool(prioritise_lower_judgements)
            )
                .Pos(690.0f)
                .Tooltip(Tooltip.Info("hud.judgementmeter.prioritiselowerjudgements"))
            |+ preview
        )

    override this.Title = %"hud.judgementmeter.name"
    override this.OnDestroy() = preview.Destroy()

    override this.OnClose() =
        HUDOptions.set<HUD.JudgementMeter>
            {
                Position = pos.Value
                AnimationTime = animation_time.Value
                IgnorePerfectJudgements = ignore_perfect_judgements.Value
                PrioritiseLowerJudgements = prioritise_lower_judgements.Value
            }

type EditEarlyLateMeterPage() as this =
    inherit Page()

    let data = HUDOptions.get<HUD.EarlyLateMeter> ()

    let pos = Setting.simple data.Position
    let default_pos = HUD.EarlyLateMeter.Default.Position

    let animation_time =
        Setting.simple data.AnimationTime |> Setting.bound 100.0f 2000.0f

    let early_text = Setting.simple data.EarlyText
    let late_text = Setting.simple data.LateText
    let early_color = Setting.simple data.EarlyColor
    let late_color = Setting.simple data.LateColor

    let preview =
        { new ConfigPreview(0.5f, pos) with
            override this.DrawComponent(bounds) =
                Text.fill (Style.font, early_text.Value, bounds, early_color.Value, Alignment.CENTER)
        }

    do
        this.Content(
            position_editor pos default_pos
            |+ PageSetting("hud.earlylatemeter.animationtime", Slider(animation_time, Step = 5f))
                .Pos(550.0f)
                .Tooltip(Tooltip.Info("hud.earlylatemeter.animationtime"))
            |+ PageTextEntry("hud.earlylatemeter.earlytext", early_text)
                .Pos(620.0f)
                .Tooltip(Tooltip.Info("hud.earlylatemeter.earlytext"))
            |+ PageSetting("hud.earlylatemeter.earlycolor", ColorPicker(early_color, false))
                .Pos(690.0f, PRETTYWIDTH, PRETTYHEIGHT * 1.5f)
                .Tooltip(Tooltip.Info("hud.earlylatemeter.earlycolor"))
            |+ PageTextEntry("hud.earlylatemeter.latetext", late_text)
                .Pos(795.0f)
                .Tooltip(Tooltip.Info("hud.earlylatemeter.latetext"))
            |+ PageSetting("hud.earlylatemeter.latecolor", ColorPicker(late_color, false))
                .Pos(865.0f, PRETTYWIDTH, PRETTYHEIGHT * 1.5f)
                .Tooltip(Tooltip.Info("hud.earlylatemeter.latecolor"))
            |+ preview
        )

    override this.Title = %"hud.earlylatemeter.name"
    override this.OnDestroy() = preview.Destroy()

    override this.OnClose() =
        HUDOptions.set<HUD.EarlyLateMeter>
            {
                Position = pos.Value
                AnimationTime = animation_time.Value
                EarlyText = early_text.Value
                EarlyColor = early_color.Value
                LateText = late_text.Value
                LateColor = late_color.Value
            }

type RateModMeterPage() as this =
    inherit Page()

    let data = HUDOptions.get<HUD.RateModMeter> ()

    let pos = Setting.simple data.Position
    let default_pos = HUD.RateModMeter.Default.Position

    let show_mods = Setting.simple data.ShowMods

    let preview =
        { new ConfigPreview(0.5f, pos) with
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
            position_editor pos default_pos
            |+ PageSetting("hud.ratemodmeter.showmods", Selector<_>.FromBool(show_mods))
                .Pos(550.0f)
                .Tooltip(Tooltip.Info("hud.ratemodmeter.showmods"))
            |+ preview
        )

    override this.Title = %"hud.ratemodmeter.name"
    override this.OnDestroy() = preview.Destroy()

    override this.OnClose() =
        HUDOptions.set<HUD.RateModMeter>
            {
                Position = pos.Value
                ShowMods = show_mods.Value
            }

type BPMMeterPage() as this =
    inherit Page()

    let data = HUDOptions.get<HUD.BPMMeter> ()

    let pos = Setting.simple data.Position
    let default_pos = HUD.BPMMeter.Default.Position

    let preview =
        { new ConfigPreview(0.5f, pos) with
            override this.DrawComponent(bounds) =
                Text.fill_b (Style.font, "727 BPM", bounds, Colors.text_subheading, Alignment.CENTER)
        }

    do this.Content(position_editor pos default_pos |+ preview)

    override this.Title = %"hud.bpmmeter.name"
    override this.OnDestroy() = preview.Destroy()

    override this.OnClose() =
        HUDOptions.set<HUD.BPMMeter> { Position = pos.Value }

type EditHUDPage() as this =
    inherit Page()

    let grid =
        GridFlowContainer<_>(PRETTYHEIGHT, 2, Spacing = (20.0f, 20.0f), Position = Position.Margin(100.0f, 200.0f))

    do
        this.Content(
            grid
            |+ PageButton("hud.accuracymeter", (fun () -> Menu.ShowPage EditAccuracyMeterPage))
                .Tooltip(Tooltip.Info("hud.accuracymeter"))
            |+ PageButton("hud.hitmeter", (fun () -> Menu.ShowPage EditHitMeterPage))
                .Tooltip(Tooltip.Info("hud.hitmeter"))
            |+ PageButton("hud.combo", (fun () -> Menu.ShowPage EditComboMeterPage))
                .Tooltip(Tooltip.Info("hud.combo"))
            |+ PageButton("hud.skipbutton", (fun () -> Menu.ShowPage EditSkipButtonPage))
                .Tooltip(Tooltip.Info("hud.skipbutton"))
            |+ PageButton("hud.progressmeter", (fun () -> Menu.ShowPage EditProgressMeterPage))
                .Tooltip(Tooltip.Info("hud.progressmeter"))
            |+ PageButton("hud.pacemaker", (fun () -> Menu.ShowPage EditPacemakerPage))
                .Tooltip(Tooltip.Info("hud.pacemaker"))
            |+ PageButton("hud.judgementcounts", (fun () -> Menu.ShowPage EditJudgementCountsPage))
                .Tooltip(Tooltip.Info("hud.judgementcounts"))
            |+ PageButton("hud.judgementmeter", (fun () -> Menu.ShowPage EditJudgementMeterPage))
                .Tooltip(Tooltip.Info("hud.judgementmeter"))
            |+ PageButton("hud.earlylatemeter", (fun () -> Menu.ShowPage EditEarlyLateMeterPage))
                .Tooltip(Tooltip.Info("hud.earlylatemeter"))
            |+ PageButton("hud.ratemodmeter", (fun () -> Menu.ShowPage RateModMeterPage))
                .Tooltip(Tooltip.Info("hud.ratemodmeter"))
            |+ PageButton("hud.bpmmeter", (fun () -> Menu.ShowPage BPMMeterPage))
                .Tooltip(Tooltip.Info("hud.bpmmeter"))
        )

    override this.Title = %"hud.name"
    override this.OnClose() = ()
