namespace Interlude.Features.OptionsMenu.Themes

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Prelude.Common
open Prelude.Data.Themes
open Prelude.Data.Themes.WidgetConfig
open Interlude.Content
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Menu

[<AutoOpen>]
module private Helpers =

    [<AbstractClass>]
    type PositionEditor(icon: string) =
        inherit StaticContainer(NodeType.Leaf)

        override this.Init(parent) =
            base.Init parent
            this
            |+ Text(icon, Align = Alignment.LEFT)
            |* Clickable.Focus this

        override this.Update(elapsedTime, moved) =
            base.Update(elapsedTime, moved)

            if this.Selected then
                if (!|"up").Tapped() then this.Up()
                elif (!|"down").Tapped() then this.Down()
                elif (!|"left").Tapped() then this.Left()
                elif (!|"right").Tapped() then this.Right()
        
        abstract member Up : unit -> unit
        abstract member Down : unit -> unit
        abstract member Left : unit -> unit
        abstract member Right : unit -> unit
     
    let positionEditor (setting: Setting<WidgetConfig>) (default_pos: WidgetConfig) =
        column()
        |+ PageSetting(
            "themes.edittheme.gameplay.generic.enable",
            Selector<_>.FromBool(
                Setting.make (fun v -> setting.Set { setting.Value with Enabled = v } ) (fun () -> setting.Value.Enabled)
            ) ).Pos(100.0f)
        |+ PageSetting(
            "themes.edittheme.gameplay.generic.float",
            Selector<_>.FromBool(
                Setting.make (fun v -> setting.Set { setting.Value with Float = v } ) (fun () -> setting.Value.Float)
            ) )
            .Pos(170.0f)
            .Tooltip(Tooltip.Info("themes.edittheme.gameplay.generic.float"))

        |+ PageSetting(
            "themes.edittheme.gameplay.generic.move",
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
            .Tooltip(Tooltip.Info("themes.edittheme.gameplay.generic.move"))
        
        |+ PageSetting(
            "themes.edittheme.gameplay.generic.grow",
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
            .Tooltip(Tooltip.Info("themes.edittheme.gameplay.generic.grow"))

        |+ PageSetting(
            "themes.edittheme.gameplay.generic.shrink",
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
            .Tooltip(Tooltip.Info("themes.edittheme.gameplay.generic.shrink"))
        
        |+ PageButton(
            "themes.edittheme.gameplay.generic.reset",
            fun () -> setting.Value <- { default_pos with Enabled = setting.Value.Enabled }
           )
           .Pos(450.0f)
           .Tooltip(Tooltip.Info("themes.edittheme.gameplay.generic.reset"))

type EditAccuracyMeterPage() as this =
    inherit Page()

    let data = Themes.Current.GameplayConfig.get<AccuracyMeter>()

    let pos = Setting.simple data.Position
    let default_pos = AccuracyMeter.Default.Position

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
                "themes.edittheme.gameplay.accuracymeter.gradecolors",
                Selector<_>.FromBool grade_colors )
                .Pos(550.0f)
                .Tooltip(Tooltip.Info("themes.edittheme.gameplay.accuracymeter.gradecolors"))
            |+ PageSetting(
                "themes.edittheme.gameplay.accuracymeter.showname",
                Selector<_>.FromBool show_name )
                .Pos(620.0f)
                .Tooltip(Tooltip.Info("themes.edittheme.gameplay.accuracymeter.showname"))
            |+ preview
        )

    override this.Title = L"themes.edittheme.gameplay.accuracymeter.name"
    override this.OnDestroy() = preview.Destroy()
    override this.OnClose() = 
        Themes.Current.GameplayConfig.set<AccuracyMeter>
            { data with
                Position = pos.Value
                GradeColors = grade_colors.Value
                ShowName = show_name.Value
            }

type EditHitMeterPage() as this =
    inherit Page()

    let data = Themes.Current.GameplayConfig.get<HitMeter>()

    let pos = Setting.simple data.Position
    let default_pos = HitMeter.Default.Position

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
                "themes.edittheme.gameplay.hitmeter.showguide",
                Selector<_>.FromBool show_guide )
                .Pos(550.0f)
                .Tooltip(Tooltip.Info("themes.edittheme.gameplay.hitmeter.showguide"))
            |+ PageSetting(
                "themes.edittheme.gameplay.hitmeter.shownonjudgements",
                Selector<_>.FromBool show_non_judgements )
                .Pos(620.0f)
                .Tooltip(Tooltip.Info("themes.edittheme.gameplay.hitmeter.shownonjudgements"))
            |+ PageSetting(
                "themes.edittheme.gameplay.hitmeter.thickness",
                Slider<float32>(thickness, 0.1f) )
                .Pos(690.0f)
                .Tooltip(Tooltip.Info("themes.edittheme.gameplay.hitmeter.thickness"))
            |+ PageSetting(
                "themes.edittheme.gameplay.hitmeter.releasesextraheight",
                Slider<float32>(release_thickness, 0.1f) )
                .Pos(760.0f)
                .Tooltip(Tooltip.Info("themes.edittheme.gameplay.hitmeter.releasesextraheight"))
            |+ PageSetting(
                "themes.edittheme.gameplay.hitmeter.animationtime",
                Slider<float32>(animation_time, 0.1f) )
                .Pos(830.0f)
                .Tooltip(Tooltip.Info("themes.edittheme.gameplay.hitmeter.animationtime"))
            |+ preview
        )

    override this.Title = L"themes.edittheme.gameplay.hitmeter.name"
    override this.OnDestroy() = preview.Destroy()
    override this.OnClose() = 
        Themes.Current.GameplayConfig.set<HitMeter>
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

    let data = Themes.Current.GameplayConfig.get<LifeMeter>()

    let pos = Setting.simple data.Position
    let default_pos = LifeMeter.Default.Position

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
                "themes.edittheme.gameplay.lifemeter.horizontal",
                Selector<_>.FromBool horizontal )
                .Pos(550.0f)
                .Tooltip(Tooltip.Info("themes.edittheme.gameplay.lifemeter.horizontal"))
            |+ PageSetting(
                "themes.edittheme.gameplay.lifemeter.fullcolor",
                ColorPicker(full_color, false) )
                .Pos(620.0f, PRETTYWIDTH, PRETTYHEIGHT * 1.5f)
                .Tooltip(Tooltip.Info("themes.edittheme.gameplay.lifemeter.fullcolor"))
            |+ PageSetting(
                "themes.edittheme.gameplay.lifemeter.emptycolor",
                ColorPicker(empty_color, false) )
                .Pos(725.0f, PRETTYWIDTH, PRETTYHEIGHT * 1.5f)
                .Tooltip(Tooltip.Info("themes.edittheme.gameplay.lifemeter.emptycolor"))
            |+ PageSetting(
                "themes.edittheme.gameplay.lifemeter.tipcolor",
                ColorPicker(tip_color, true) )
                .Pos(830.0f, PRETTYWIDTH, PRETTYHEIGHT * 1.5f)
                .Tooltip(Tooltip.Info("themes.edittheme.gameplay.lifemeter.tipcolor"))
            |+ preview
        )

    override this.Title = L"themes.edittheme.gameplay.lifemeter.name"
    override this.OnDestroy() = preview.Destroy()
    override this.OnClose() = 
        Themes.Current.GameplayConfig.set<LifeMeter>
            { data with
                Position = pos.Value
                Horizontal = horizontal.Value
                FullColor = full_color.Value
                EmptyColor = empty_color.Value
                TipColor = tip_color.Value
            }

type EditComboMeterPage() as this =
    inherit Page()

    let data = Themes.Current.GameplayConfig.get<Combo>()

    let pos = Setting.simple data.Position
    let default_pos = Combo.Default.Position

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
                "themes.edittheme.gameplay.combo.lampcolors",
                Selector<_>.FromBool lamp_colors )
                .Pos(550.0f)
                .Tooltip(Tooltip.Info("themes.edittheme.gameplay.combo.lampcolors"))
            |+ PageSetting(
                "themes.edittheme.gameplay.combo.pop",
                Slider(pop_amount, 0.1f) )
                .Pos(620.0f)
                .Tooltip(Tooltip.Info("themes.edittheme.gameplay.combo.pop"))
            |+ PageSetting(
                "themes.edittheme.gameplay.combo.growth",
                Slider(growth_amount, 0.2f) )
                .Pos(690.0f)
                .Tooltip(Tooltip.Info("themes.edittheme.gameplay.combo.growth"))
            |+ preview
        )

    override this.Title = L"themes.edittheme.gameplay.combo.name"
    override this.OnDestroy() = preview.Destroy()
    override this.OnClose() = 
        Themes.Current.GameplayConfig.set<Combo>
            { data with
                Position = pos.Value
                LampColors = lamp_colors.Value
                Pop = pop_amount.Value
                Growth = growth_amount.Value
            }

type EditSkipButtonPage() as this =
    inherit Page()

    let data = Themes.Current.GameplayConfig.get<SkipButton>()

    let pos = Setting.simple data.Position
    let default_pos = SkipButton.Default.Position

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

    override this.Title = L"themes.edittheme.gameplay.skipbutton.name"
    override this.OnDestroy() = preview.Destroy()
    override this.OnClose() = 
        Themes.Current.GameplayConfig.set<SkipButton>
            { data with Position = pos.Value }

type EditProgressMeterPage() as this =
    inherit Page()

    let data = Themes.Current.GameplayConfig.get<ProgressMeter>()

    let pos = Setting.simple data.Position
    let default_pos = ProgressMeter.Default.Position

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
                "themes.edittheme.gameplay.progressmeter.barheight",
                Slider(bar_height, 0.1f))
                .Pos(550.0f)
                .Tooltip(Tooltip.Info("themes.edittheme.gameplay.progressmeter.barheight"))
            |+ PageSetting(
                "themes.edittheme.gameplay.progressmeter.barcolor",
                ColorPicker(bar_color, true) )
                .Pos(620.0f, PRETTYWIDTH, PRETTYHEIGHT * 1.5f)
                .Tooltip(Tooltip.Info("themes.edittheme.gameplay.progressmeter.barcolor"))
            |+ PageSetting(
                "themes.edittheme.gameplay.progressmeter.glowsize",
                Slider(glow_size, 0.1f) )
                .Pos(725.0f)
                .Tooltip(Tooltip.Info("themes.edittheme.gameplay.progressmeter.glowsize"))
            |+ PageSetting(
                "themes.edittheme.gameplay.progressmeter.glowcolor",
                ColorPicker(glow_color, true) )
                .Pos(830.0f, PRETTYWIDTH, PRETTYHEIGHT * 1.5f)
                .Tooltip(Tooltip.Info("themes.edittheme.gameplay.progressmeter.glowcolor"))
            |+ preview
        )

    override this.Title = L"themes.edittheme.gameplay.progressmeter.name"
    override this.OnDestroy() = preview.Destroy()
    override this.OnClose() = 
        Themes.Current.GameplayConfig.set<ProgressMeter>
            { data with
                Position = pos.Value
                BarHeight = bar_height.Value
                BarColor = bar_color.Value
                GlowSize = glow_size.Value
                GlowColor = glow_color.Value
            }

type EditPacemakerPage() as this =
    inherit Page()

    let data = Themes.Current.GameplayConfig.get<Pacemaker>()

    let pos = Setting.simple data.Position
    let default_pos = Pacemaker.Default.Position

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

    override this.Title = L"themes.edittheme.gameplay.pacemaker.name"
    override this.OnDestroy() = preview.Destroy()
    override this.OnClose() = 
        Themes.Current.GameplayConfig.set<Pacemaker>
            { data with Position = pos.Value }
            
type EditJudgementCountsPage() as this =
    inherit Page()
            
    let data = Themes.Current.GameplayConfig.get<JudgementCounts>()
            
    let pos = Setting.simple data.Position
    let default_pos = JudgementCounts.Default.Position

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
                "themes.edittheme.gameplay.judgementcounts.animationtime",
                Slider<float>(animation_time, 0.1f) )
                .Pos(550.0f)
                .Tooltip(Tooltip.Info("themes.edittheme.gameplay.judgementcounts.animationtime"))
            |+ preview
        )
            
    override this.Title = L"themes.edittheme.gameplay.judgementcounts.name"
    override this.OnDestroy() = preview.Destroy()
    override this.OnClose() = 
        Themes.Current.GameplayConfig.set<JudgementCounts>
            { data with Position = pos.Value; AnimationTime = animation_time.Value }

type EditGameplayConfigPage() as this =
    inherit Page()

    do
        this.Content(
            column()
            |+ PageButton("themes.edittheme.gameplay.accuracymeter", fun () -> Menu.ShowPage EditAccuracyMeterPage)
                .Pos(200.0f)
                .Tooltip(Tooltip.Info("themes.edittheme.gameplay.accuracymeter"))
            |+ PageButton("themes.edittheme.gameplay.hitmeter", fun () -> Menu.ShowPage EditHitMeterPage)
                .Pos(270.0f)
                .Tooltip(Tooltip.Info("themes.edittheme.gameplay.hitmeter"))
            |+ PageButton("themes.edittheme.gameplay.lifemeter", fun () -> Menu.ShowPage EditLifeMeterPage)
                .Pos(340.0f)
                .Tooltip(Tooltip.Info("themes.edittheme.gameplay.lifemeter"))
            |+ PageButton("themes.edittheme.gameplay.combo", fun () -> Menu.ShowPage EditComboMeterPage)
                .Pos(410.0f)
                .Tooltip(Tooltip.Info("themes.edittheme.gameplay.combo"))
            |+ PageButton("themes.edittheme.gameplay.skipbutton", fun () -> Menu.ShowPage EditSkipButtonPage)
                .Pos(480.0f)
                .Tooltip(Tooltip.Info("themes.edittheme.gameplay.skipbutton"))
            |+ PageButton("themes.edittheme.gameplay.progressmeter", fun () -> Menu.ShowPage EditProgressMeterPage)
                .Pos(550.0f)
                .Tooltip(Tooltip.Info("themes.edittheme.gameplay.progressmeter"))
            |+ PageButton("themes.edittheme.gameplay.pacemaker", fun () -> Menu.ShowPage EditPacemakerPage)
                .Pos(620.0f)
                .Tooltip(Tooltip.Info("themes.edittheme.gameplay.pacemaker"))
            |+ PageButton("themes.edittheme.gameplay.judgementcounts", fun () -> Menu.ShowPage EditJudgementCountsPage)
                .Pos(690.0f)
                .Tooltip(Tooltip.Info("themes.edittheme.gameplay.judgementcounts"))
        )

    override this.Title = L"themes.edittheme.gameplay.name"
    override this.OnClose() = ()

type EditThemePage() as this =
    inherit Page()

    let data = Themes.Current.config
        
    let name = Setting.simple data.Name

    do
        this.Content(
            column()
            |+ PageSetting("themes.edittheme.themename", TextEntry(name, "none"))
                .Pos(200.0f)
            |+ PageButton("themes.edittheme.gameplay", fun () -> Menu.ShowPage EditGameplayConfigPage)
                .Pos(300.0f)
                .Tooltip(Tooltip.Info("themes.edittheme.gameplay"))
        )

    override this.Title = data.Name
    override this.OnClose() =
        Themes.Current.changeConfig
            { data with
                Name = name.Value
            }