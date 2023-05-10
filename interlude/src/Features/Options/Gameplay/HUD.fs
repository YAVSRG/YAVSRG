namespace Interlude.Features.OptionsMenu.Gameplay

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
open Interlude.Features.OptionsMenu.Themes

[<AutoOpen>]
module private Helpers =

    [<AbstractClass>]
    type PositionEditor(icon: string) =
        inherit StaticContainer(NodeType.Leaf)

        let mutable repeat = -1
        let mutable time = 0.0
        let REPEAT_DELAY = 400.0
        let REPEAT_INTERVAL = 40.0

        override this.Init(parent) =
            base.Init parent
            this
            |+ Text(icon, Align = Alignment.LEFT)
            |* Clickable.Focus this

        override this.Update(elapsedTime, moved) =
            base.Update(elapsedTime, moved)

            if this.Selected then
                let u = (!|"up").Tapped()
                let d = (!|"down").Tapped()
                let l = (!|"left").Tapped()
                let r = (!|"right").Tapped()

                if u || d || l || r then
                    repeat <- 0
                    time <- 0
                    if u then this.Up()
                    if d then this.Down()
                    if l then this.Left()
                    if r then this.Right()

                if repeat >= 0 then
                    let u = (!|"up").Pressed()
                    let d = (!|"down").Pressed()
                    let l = (!|"left").Pressed()
                    let r = (!|"right").Pressed()

                    time <- time + elapsedTime
                    if (float repeat * REPEAT_INTERVAL + REPEAT_DELAY < time) then
                        repeat <- repeat + 1
                        if u then this.Up()
                        if d then this.Down()
                        if l then this.Left()
                        if r then this.Right()
                    
                    if not (u || d || l || r) then repeat <- -1
        
        abstract member Up : unit -> unit
        abstract member Down : unit -> unit
        abstract member Left : unit -> unit
        abstract member Right : unit -> unit
     
    let positionEditor (setting: Setting<WidgetPosition>) (default_pos: WidgetPosition) =
        column()
        |+ PageSetting(
            "gameplay.hud.generic.enable",
            Selector<_>.FromBool(
                Setting.make (fun v -> setting.Set { setting.Value with Enabled = v } ) (fun () -> setting.Value.Enabled)
            ) ).Pos(100.0f)
        |+ PageSetting(
            "gameplay.hud.generic.float",
            Selector<_>.FromBool(
                Setting.make (fun v -> setting.Set { setting.Value with Float = v } ) (fun () -> setting.Value.Float)
            ) )
            .Pos(170.0f)
            .Tooltip(Tooltip.Info("gameplay.hud.generic.float"))

        |+ PageSetting(
            "gameplay.hud.generic.move",
            { new PositionEditor(Icons.move) with
                override this.Up() =
                    { setting.Value with 
                        Top = setting.Value.Top - 5.0f
                        Bottom = setting.Value.Bottom - 5.0f
                    } |> setting.Set
                override this.Down() =
                    { setting.Value with 
                        Top = setting.Value.Top + 5.0f
                        Bottom = setting.Value.Bottom + 5.0f
                    } |> setting.Set
                override this.Left() =
                    { setting.Value with 
                        Left = setting.Value.Left - 5.0f
                        Right = setting.Value.Right - 5.0f
                    } |> setting.Set
                override this.Right() =
                    { setting.Value with 
                        Left = setting.Value.Left + 5.0f
                        Right = setting.Value.Right + 5.0f
                    } |> setting.Set
            } )
            .Pos(240.0f)
            .Tooltip(Tooltip.Info("gameplay.hud.generic.move"))
        
        |+ PageSetting(
            "gameplay.hud.generic.grow",
            { new PositionEditor(Icons.grow) with
                override this.Up() =
                    { setting.Value with 
                        Top = setting.Value.Top - 5.0f
                    } |> setting.Set
                override this.Down() =
                    { setting.Value with 
                        Bottom = setting.Value.Bottom + 5.0f
                    } |> setting.Set
                override this.Left() =
                    { setting.Value with 
                        Left = setting.Value.Left - 5.0f
                    } |> setting.Set
                override this.Right() =
                    { setting.Value with 
                        Right = setting.Value.Right + 5.0f
                    } |> setting.Set
            } )
            .Pos(310.0f)
            .Tooltip(Tooltip.Info("gameplay.hud.generic.grow"))

        |+ PageSetting(
            "gameplay.hud.generic.shrink",
            { new PositionEditor(Icons.shrink) with
                override this.Up() =
                    { setting.Value with 
                        Bottom = setting.Value.Bottom - 5.0f
                    } |> setting.Set
                override this.Down() =
                    { setting.Value with 
                        Top = setting.Value.Top + 5.0f
                    } |> setting.Set
                override this.Left() =
                    { setting.Value with 
                        Right = setting.Value.Right - 5.0f
                    } |> setting.Set
                override this.Right() =
                    { setting.Value with 
                        Left = setting.Value.Left + 5.0f
                    } |> setting.Set
            } )
            .Pos(380.0f)
            .Tooltip(Tooltip.Info("gameplay.hud.generic.shrink"))
        
        |+ PageButton(
            "gameplay.hud.generic.reset",
            fun () -> setting.Value <- { default_pos with Enabled = setting.Value.Enabled }
           )
           .Pos(450.0f)
           .Tooltip(Tooltip.Info("gameplay.hud.generic.reset"))

type EditAccuracyMeterPage() as this =
    inherit Page()

    let data = HUDOptions.get<HUD.AccuracyMeter>()

    let pos = Setting.simple data.Position
    let default_pos = HUD.AccuracyMeter.Default.Position

    let grade_colors = Setting.simple data.GradeColors
    let show_name = Setting.simple data.ShowName

    let preview = 
        { new ConfigPreview(0.5f, pos) with
            override this.DrawComponent(bounds) =
                Text.drawFill(Style.baseFont, "96.72%", bounds.TrimBottom(bounds.Height * 0.3f), Color.White, 0.5f)
                if show_name.Value then Text.drawFill(Style.baseFont, "SC+ J4", bounds.SliceBottom(bounds.Height * 0.4f), Color.White, 0.5f)
        }

    do
        this.Content(
            positionEditor pos default_pos
            |+ PageSetting(
                "gameplay.hud.accuracymeter.gradecolors",
                Selector<_>.FromBool grade_colors )
                .Pos(550.0f)
                .Tooltip(Tooltip.Info("gameplay.hud.accuracymeter.gradecolors"))
            |+ PageSetting(
                "gameplay.hud.accuracymeter.showname",
                Selector<_>.FromBool show_name )
                .Pos(620.0f)
                .Tooltip(Tooltip.Info("gameplay.hud.accuracymeter.showname"))
            |+ preview
        )

    override this.Title = L"gameplay.hud.accuracymeter.name"
    override this.OnDestroy() = preview.Destroy()
    override this.OnClose() = 
        HUDOptions.set<HUD.AccuracyMeter>
            { data with
                Position = pos.Value
                GradeColors = grade_colors.Value
                ShowName = show_name.Value
            }

type EditHitMeterPage() as this =
    inherit Page()

    let data = HUDOptions.get<HUD.HitMeter>()

    let pos = Setting.simple data.Position
    let default_pos = HUD.HitMeter.Default.Position

    let show_guide = Setting.simple data.ShowGuide
    let show_non_judgements = Setting.simple data.ShowNonJudgements
    let thickness = Setting.simple data.Thickness |> Setting.bound 5.0f 25.0f
    let release_thickness = Setting.simple data.ReleasesExtraHeight |> Setting.bound 0.0f 20.0f
    let animation_time = Setting.simple data.AnimationTime |> Setting.bound 100.0f 2000.0f

    let preview = 
        { new ConfigPreview(0.5f, pos) with
            override this.DrawComponent(bounds) =
                Draw.rect
                    (Rect.Create(bounds.CenterX - thickness.Value / 2.0f, bounds.Top, bounds.CenterX + thickness.Value / 2.0f, bounds.Bottom))
                    Color.White
        }

    do
        this.Content(
            positionEditor pos default_pos
            |+ PageSetting(
                "gameplay.hud.hitmeter.showguide",
                Selector<_>.FromBool show_guide )
                .Pos(550.0f)
                .Tooltip(Tooltip.Info("gameplay.hud.hitmeter.showguide"))
            |+ PageSetting(
                "gameplay.hud.hitmeter.shownonjudgements",
                Selector<_>.FromBool show_non_judgements )
                .Pos(620.0f)
                .Tooltip(Tooltip.Info("gameplay.hud.hitmeter.shownonjudgements"))
            |+ PageSetting(
                "gameplay.hud.hitmeter.thickness",
                Slider(thickness, Step = 1f) )
                .Pos(690.0f)
                .Tooltip(Tooltip.Info("gameplay.hud.hitmeter.thickness"))
            |+ PageSetting(
                "gameplay.hud.hitmeter.releasesextraheight",
                Slider(release_thickness, Step = 1f) )
                .Pos(760.0f)
                .Tooltip(Tooltip.Info("gameplay.hud.hitmeter.releasesextraheight"))
            |+ PageSetting(
                "gameplay.hud.hitmeter.animationtime",
                Slider(animation_time, Step = 5f) )
                .Pos(830.0f)
                .Tooltip(Tooltip.Info("gameplay.hud.hitmeter.animationtime"))
            |+ preview
        )

    override this.Title = L"gameplay.hud.hitmeter.name"
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

type EditLifeMeterPage() as this =
    inherit Page()

    let data = HUDOptions.get<HUD.LifeMeter>()

    let pos = Setting.simple data.Position
    let default_pos = HUD.LifeMeter.Default.Position

    let horizontal = Setting.simple data.Horizontal
    let empty_color = Setting.simple data.EmptyColor
    let full_color = Setting.simple data.FullColor
    let tip_color = Setting.simple data.TipColor

    let preview = 
        { new ConfigPreview(0.5f, pos) with
            override this.DrawComponent(bounds) =
                if horizontal.Value then
                    Draw.rect bounds (full_color.Value)
                    Draw.rect (bounds.SliceRight bounds.Height) tip_color.Value
                else
                    Draw.rect bounds (full_color.Value)
                    Draw.rect (bounds.SliceTop bounds.Width) tip_color.Value
        }

    do
        this.Content(
            positionEditor pos default_pos
            |+ PageSetting(
                "gameplay.hud.lifemeter.horizontal",
                Selector<_>.FromBool horizontal )
                .Pos(550.0f)
                .Tooltip(Tooltip.Info("gameplay.hud.lifemeter.horizontal"))
            |+ PageSetting(
                "gameplay.hud.lifemeter.fullcolor",
                ColorPicker(full_color, false) )
                .Pos(620.0f, PRETTYWIDTH, PRETTYHEIGHT * 1.5f)
                .Tooltip(Tooltip.Info("gameplay.hud.lifemeter.fullcolor"))
            |+ PageSetting(
                "gameplay.hud.lifemeter.emptycolor",
                ColorPicker(empty_color, false) )
                .Pos(725.0f, PRETTYWIDTH, PRETTYHEIGHT * 1.5f)
                .Tooltip(Tooltip.Info("gameplay.hud.lifemeter.emptycolor"))
            |+ PageSetting(
                "gameplay.hud.lifemeter.tipcolor",
                ColorPicker(tip_color, true) )
                .Pos(830.0f, PRETTYWIDTH, PRETTYHEIGHT * 1.5f)
                .Tooltip(Tooltip.Info("gameplay.hud.lifemeter.tipcolor"))
            |+ preview
        )

    override this.Title = L"gameplay.hud.lifemeter.name"
    override this.OnDestroy() = preview.Destroy()
    override this.OnClose() = 
        HUDOptions.set<HUD.LifeMeter>
            { data with
                Position = pos.Value
                Horizontal = horizontal.Value
                FullColor = full_color.Value
                EmptyColor = empty_color.Value
                TipColor = tip_color.Value
            }

type EditComboMeterPage() as this =
    inherit Page()

    let data = HUDOptions.get<HUD.Combo>()

    let pos = Setting.simple data.Position
    let default_pos = HUD.Combo.Default.Position

    let lamp_colors = Setting.simple data.LampColors
    let pop_amount = Setting.simple data.Pop |> Setting.bound 0.0f 20.0f
    let growth_amount = Setting.simple data.Growth |> Setting.bound 0.0f 0.05f

    let preview = 
        { new ConfigPreview(0.5f, pos) with
            override this.DrawComponent(bounds) =
                Text.drawFill(Style.baseFont, "727", bounds, Color.White, Alignment.CENTER)
        }

    do
        this.Content(
            positionEditor pos default_pos
            |+ PageSetting(
                "gameplay.hud.combo.lampcolors",
                Selector<_>.FromBool lamp_colors )
                .Pos(550.0f)
                .Tooltip(Tooltip.Info("gameplay.hud.combo.lampcolors"))
            |+ PageSetting(
                "gameplay.hud.combo.pop",
                Slider(pop_amount, Step = 1f) )
                .Pos(620.0f)
                .Tooltip(Tooltip.Info("gameplay.hud.combo.pop"))
            |+ PageSetting(
                "gameplay.hud.combo.growth",
                Slider(growth_amount) )
                .Pos(690.0f)
                .Tooltip(Tooltip.Info("gameplay.hud.combo.growth"))
            |+ preview
        )

    override this.Title = L"gameplay.hud.combo.name"
    override this.OnDestroy() = preview.Destroy()
    override this.OnClose() = 
        HUDOptions.set<HUD.Combo>
            { data with
                Position = pos.Value
                LampColors = lamp_colors.Value
                Pop = pop_amount.Value
                Growth = growth_amount.Value
            }

type EditSkipButtonPage() as this =
    inherit Page()

    let data = HUDOptions.get<HUD.SkipButton>()

    let pos = Setting.simple data.Position
    let default_pos = HUD.SkipButton.Default.Position

    let preview_text = Localisation.localiseWith [(!|"skip").ToString()] "play.skiphint"
    let preview = 
        { new ConfigPreview(0.5f, pos) with
            override this.DrawComponent(bounds) =
                Text.drawFillB(Style.baseFont, preview_text, bounds, Style.text(), Alignment.CENTER)
        }

    do
        this.Content(
            positionEditor pos default_pos
            |+ preview
        )

    override this.Title = L"gameplay.hud.skipbutton.name"
    override this.OnDestroy() = preview.Destroy()
    override this.OnClose() = 
        HUDOptions.set<HUD.SkipButton>
            { data with Position = pos.Value }

type EditProgressMeterPage() as this =
    inherit Page()

    let data = HUDOptions.get<HUD.ProgressMeter>()

    let pos = Setting.simple data.Position
    let default_pos = HUD.ProgressMeter.Default.Position

    let bar_color = Setting.simple data.BarColor
    let glow_color = Setting.simple data.GlowColor
    let bar_height = Setting.simple data.BarHeight |> Setting.bound 0.0f 100.0f
    let glow_size = Setting.simple data.GlowSize |> Setting.bound 0.0f 20.0f

    let preview = 
        { new ConfigPreview(0.5f, pos) with
            override this.DrawComponent(bounds) =
                let bar = bounds.SliceTop(150.0f).SliceBottom(0.5f * bar_height.Value)
                Draw.rect (bar.Expand glow_size.Value) (glow_color.Value)
                Draw.rect bar bar_color.Value
        }

    do
        this.Content(
            positionEditor pos default_pos
            |+ PageSetting(
                "gameplay.hud.progressmeter.barheight",
                Slider(bar_height, Step = 1f))
                .Pos(550.0f)
                .Tooltip(Tooltip.Info("gameplay.hud.progressmeter.barheight"))
            |+ PageSetting(
                "gameplay.hud.progressmeter.barcolor",
                ColorPicker(bar_color, true) )
                .Pos(620.0f, PRETTYWIDTH, PRETTYHEIGHT * 1.5f)
                .Tooltip(Tooltip.Info("gameplay.hud.progressmeter.barcolor"))
            |+ PageSetting(
                "gameplay.hud.progressmeter.glowsize",
                Slider(glow_size, Step = 1f) )
                .Pos(725.0f)
                .Tooltip(Tooltip.Info("gameplay.hud.progressmeter.glowsize"))
            |+ PageSetting(
                "gameplay.hud.progressmeter.glowcolor",
                ColorPicker(glow_color, true) )
                .Pos(830.0f, PRETTYWIDTH, PRETTYHEIGHT * 1.5f)
                .Tooltip(Tooltip.Info("gameplay.hud.progressmeter.glowcolor"))
            |+ preview
        )

    override this.Title = L"gameplay.hud.progressmeter.name"
    override this.OnDestroy() = preview.Destroy()
    override this.OnClose() = 
        HUDOptions.set<HUD.ProgressMeter>
            { data with
                Position = pos.Value
                BarHeight = bar_height.Value
                BarColor = bar_color.Value
                GlowSize = glow_size.Value
                GlowColor = glow_color.Value
            }

type EditPacemakerPage() as this =
    inherit Page()

    let data = HUDOptions.get<HUD.Pacemaker>()

    let pos = Setting.simple data.Position
    let default_pos = HUD.Pacemaker.Default.Position

    let preview = 
        { new ConfigPreview(0.5f, pos) with
            override this.DrawComponent(bounds) =
                Text.drawFillB(Style.baseFont, Icons.goal, bounds, Style.text(), Alignment.CENTER)
        }

    do
        this.Content(
            positionEditor pos default_pos
            |+ preview
        )

    override this.Title = L"gameplay.hud.pacemaker.name"
    override this.OnDestroy() = preview.Destroy()
    override this.OnClose() = 
        HUDOptions.set<HUD.Pacemaker>
            { data with Position = pos.Value }
            
type EditJudgementCountsPage() as this =
    inherit Page()
            
    let data = HUDOptions.get<HUD.JudgementCounts>()
            
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
            positionEditor pos default_pos
            |+ PageSetting(
                "gameplay.hud.judgementcounts.animationtime",
                Slider(animation_time |> Setting.f32, Step = 5f) )
                .Pos(550.0f)
                .Tooltip(Tooltip.Info("gameplay.hud.judgementcounts.animationtime"))
            |+ preview
        )
            
    override this.Title = L"gameplay.hud.judgementcounts.name"
    override this.OnDestroy() = preview.Destroy()
    override this.OnClose() = 
        HUDOptions.set<HUD.JudgementCounts>
            { data with Position = pos.Value; AnimationTime = animation_time.Value }

type EditJudgementMeterPage() as this =
    inherit Page()

    let data = HUDOptions.get<HUD.JudgementMeter>()

    let pos = Setting.simple data.Position
    let default_pos = HUD.JudgementMeter.Default.Position

    let ignore_perfect_judgements = Setting.simple data.IgnorePerfectJudgements
    let animation_time = Setting.simple data.AnimationTime |> Setting.bound 100.0f 2000.0f
    let rs = Rulesets.current

    let preview = 
        { new ConfigPreview(0.5f, pos) with
            override this.DrawComponent(bounds) =
                Text.drawFill(Style.baseFont, rs.JudgementName 0, bounds, rs.JudgementColor 0, Alignment.CENTER)
        }

    do
        this.Content(
            positionEditor pos default_pos
            |+ PageSetting(
                "gameplay.hud.judgementmeter.animationtime",
                Slider(animation_time, Step = 5f) )
                .Pos(550.0f)
                .Tooltip(Tooltip.Info("gameplay.hud.judgementmeter.animationtime"))
            |+ PageSetting(
                "gameplay.hud.judgementmeter.ignoreperfectjudgements",
                Selector<_>.FromBool(ignore_perfect_judgements) )
                .Pos(620.0f)
                .Tooltip(Tooltip.Info("gameplay.hud.judgementmeter.ignoreperfectjudgements"))
            |+ preview
        )

    override this.Title = L"gameplay.hud.judgementmeter.name"
    override this.OnDestroy() = preview.Destroy()
    override this.OnClose() = 
        HUDOptions.set<HUD.JudgementMeter>
            { data with
                Position = pos.Value
                AnimationTime = animation_time.Value
                IgnorePerfectJudgements = ignore_perfect_judgements.Value
            }

type EditEarlyLateMeterPage() as this =
    inherit Page()

    let data = HUDOptions.get<HUD.EarlyLateMeter>()

    let pos = Setting.simple data.Position
    let default_pos = HUD.EarlyLateMeter.Default.Position

    let animation_time = Setting.simple data.AnimationTime |> Setting.bound 100.0f 2000.0f
    let early_text = Setting.simple data.EarlyText
    let late_text = Setting.simple data.LateText
    let early_color = Setting.simple data.EarlyColor
    let late_color = Setting.simple data.LateColor

    let preview = 
        { new ConfigPreview(0.5f, pos) with
            override this.DrawComponent(bounds) =
                Text.drawFill(Style.baseFont, early_text.Value, bounds, early_color.Value, Alignment.CENTER)
        }

    do
        this.Content(
            positionEditor pos default_pos
            |+ PageSetting(
                "gameplay.hud.earlylatemeter.animationtime",
                Slider(animation_time, Step = 5f) )
                .Pos(550.0f)
                .Tooltip(Tooltip.Info("gameplay.hud.earlylatemeter.animationtime"))
            |+ PageSetting(
                "gameplay.hud.earlylatemeter.earlytext",
                TextEntry(early_text, "none") )
                .Pos(620.0f)
                .Tooltip(Tooltip.Info("gameplay.hud.earlylatemeter.earlytext"))
            |+ PageSetting(
                "gameplay.hud.earlylatemeter.earlycolor",
                ColorPicker(early_color, false) )
                .Pos(690.0f, PRETTYWIDTH, PRETTYHEIGHT * 1.5f)
                .Tooltip(Tooltip.Info("gameplay.hud.earlylatemeter.earlycolor"))
            |+ PageSetting(
                "gameplay.hud.earlylatemeter.latetext",
                TextEntry(late_text, "none") )
                .Pos(795.0f)
                .Tooltip(Tooltip.Info("gameplay.hud.earlylatemeter.latetext"))
            |+ PageSetting(
                "gameplay.hud.earlylatemeter.latecolor",
                ColorPicker(late_color, false) )
                .Pos(865.0f, PRETTYWIDTH, PRETTYHEIGHT * 1.5f)
                .Tooltip(Tooltip.Info("gameplay.hud.earlylatemeter.latecolor"))
            |+ preview
        )

    override this.Title = L"gameplay.hud.earlylatemeter.name"
    override this.OnDestroy() = preview.Destroy()
    override this.OnClose() = 
        HUDOptions.set<HUD.EarlyLateMeter>
            { data with
                Position = pos.Value
                AnimationTime = animation_time.Value
                EarlyText = early_text.Value
                EarlyColor = early_color.Value
                LateText = late_text.Value
                LateColor = late_color.Value
            }

type EditHUDPage() as this =
    inherit Page()

    do
        this.Content(
            column()
            |+ PageButton("gameplay.hud.accuracymeter", fun () -> Menu.ShowPage EditAccuracyMeterPage)
                .Pos(200.0f)
                .Tooltip(Tooltip.Info("gameplay.hud.accuracymeter"))
            |+ PageButton("gameplay.hud.hitmeter", fun () -> Menu.ShowPage EditHitMeterPage)
                .Pos(270.0f)
                .Tooltip(Tooltip.Info("gameplay.hud.hitmeter"))
            |+ PageButton("gameplay.hud.lifemeter", fun () -> Menu.ShowPage EditLifeMeterPage)
                .Pos(340.0f)
                .Tooltip(Tooltip.Info("gameplay.hud.lifemeter"))
            |+ PageButton("gameplay.hud.combo", fun () -> Menu.ShowPage EditComboMeterPage)
                .Pos(410.0f)
                .Tooltip(Tooltip.Info("gameplay.hud.combo"))
            |+ PageButton("gameplay.hud.skipbutton", fun () -> Menu.ShowPage EditSkipButtonPage)
                .Pos(480.0f)
                .Tooltip(Tooltip.Info("gameplay.hud.skipbutton"))
            |+ PageButton("gameplay.hud.progressmeter", fun () -> Menu.ShowPage EditProgressMeterPage)
                .Pos(550.0f)
                .Tooltip(Tooltip.Info("gameplay.hud.progressmeter"))
            |+ PageButton("gameplay.hud.pacemaker", fun () -> Menu.ShowPage EditPacemakerPage)
                .Pos(620.0f)
                .Tooltip(Tooltip.Info("gameplay.hud.pacemaker"))
            |+ PageButton("gameplay.hud.judgementcounts", fun () -> Menu.ShowPage EditJudgementCountsPage)
                .Pos(690.0f)
                .Tooltip(Tooltip.Info("gameplay.hud.judgementcounts"))
            |+ PageButton("gameplay.hud.judgementmeter", fun () -> Menu.ShowPage EditJudgementMeterPage)
                .Pos(760.0f)
                .Tooltip(Tooltip.Info("gameplay.hud.judgementmeter"))
            |+ PageButton("gameplay.hud.earlylatemeter", fun () -> Menu.ShowPage EditEarlyLateMeterPage)
                .Pos(830.0f)
                .Tooltip(Tooltip.Info("gameplay.hud.earlylatemeter"))
        )

    override this.Title = L"gameplay.hud.name"
    override this.OnClose() = ()