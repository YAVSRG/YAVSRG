namespace Interlude.Features.OptionsMenu.Themes

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Prelude.Common
open Prelude.Data.Themes
open Prelude.Data.Themes.WidgetConfig
open Interlude.Content
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
        |+ PrettySetting(
            "themes.edittheme.gameplay.generic.enable",
            Selector<_>.FromBool(
                Setting.make (fun v -> setting.Set { setting.Value with Enabled = v } ) (fun () -> setting.Value.Enabled)
            ) ).Pos(100.0f)
        |+ PrettySetting(
            "themes.edittheme.gameplay.generic.float",
            Selector<_>.FromBool(
                Setting.make (fun v -> setting.Set { setting.Value with Float = v } ) (fun () -> setting.Value.Float)
            ) ).Pos(180.0f)

        |+ PrettySetting(
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
            } ).Pos(260.0f)
        
        |+ PrettySetting(
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
            } ).Pos(340.0f)

        |+ PrettySetting(
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
            } ).Pos(420.0f)
        
        |+ PrettyButton(
            "themes.edittheme.gameplay.generic.reset",
            fun () -> setting.Value <- { default_pos with Enabled = setting.Value.Enabled }
           ).Pos(500.0f)

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
            |+ PrettySetting(
                "themes.edittheme.gameplay.accuracymeter.gradecolors",
                Selector<_>.FromBool grade_colors ).Pos(600.0f)
            |+ PrettySetting(
                "themes.edittheme.gameplay.accuracymeter.showname",
                Selector<_>.FromBool show_name ).Pos(680.0f)
            |+ preview
        )

    override this.Title = N"themes.edittheme.gameplay.accuracymeter"
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
            |+ PrettySetting(
                "themes.edittheme.gameplay.hitmeter.showguide",
                Selector<_>.FromBool show_guide ).Pos(600.0f)
            |+ PrettySetting(
                "themes.edittheme.gameplay.hitmeter.shownonjudgements",
                Selector<_>.FromBool show_non_judgements ).Pos(680.0f)
            |+ PrettySetting(
                "themes.edittheme.gameplay.hitmeter.thickness",
                Slider<float32>(thickness, 0.1f) ).Pos(760.0f)
            |+ PrettySetting(
                "themes.edittheme.gameplay.hitmeter.releasesextraheight",
                Slider<float32>(release_thickness, 0.1f) ).Pos(840.0f)
            |+ PrettySetting(
                "themes.edittheme.gameplay.hitmeter.animationtime",
                Slider<float32>(animation_time, 0.1f) ).Pos(920.0f)
            |+ preview
        )

    override this.Title = N"themes.edittheme.gameplay.hitmeter"
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
            |+ PrettySetting(
                "themes.edittheme.gameplay.lifemeter.horizontal",
                Selector<_>.FromBool horizontal ).Pos(600.0f)
            |+ PrettySetting(
                "themes.edittheme.gameplay.lifemeter.fullcolor",
                ColorPicker(full_color, false) ).Pos(680.0f, PRETTYWIDTH, PRETTYHEIGHT * 1.5f)
            |+ PrettySetting(
                "themes.edittheme.gameplay.lifemeter.emptycolor",
                ColorPicker(empty_color, false) ).Pos(800.0f, PRETTYWIDTH, PRETTYHEIGHT * 1.5f)
            |+ PrettySetting(
                "themes.edittheme.gameplay.lifemeter.tipcolor",
                ColorPicker(tip_color, true) ).Pos(920.0f, PRETTYWIDTH, PRETTYHEIGHT * 1.5f)
            |+ preview
        )

    override this.Title = N"themes.edittheme.gameplay.lifemeter"
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

type EditGameplayConfigPage() as this =
    inherit Page()

    do
        this.Content(
            column()
            |+ PrettyButton("themes.edittheme.gameplay.accuracymeter", fun () -> Menu.ShowPage EditAccuracyMeterPage).Pos(200.0f)
            |+ PrettyButton("themes.edittheme.gameplay.hitmeter", fun () -> Menu.ShowPage EditHitMeterPage).Pos(280.0f)
            |+ PrettyButton("themes.edittheme.gameplay.lifemeter", fun () -> Menu.ShowPage EditLifeMeterPage).Pos(360.0f)
            |+ PrettyButton("themes.edittheme.gameplay.combo", ignore).Pos(440.0f)
            |+ PrettyButton("themes.edittheme.gameplay.skipbutton", ignore).Pos(520.0f)
            |+ PrettyButton("themes.edittheme.gameplay.progressmeter", ignore).Pos(600.0f)
        )

    override this.Title = N"themes.edittheme.gameplay"
    override this.OnClose() = ()

type EditThemePage(refreshThemes) as this =
    inherit Page()

    let data = Themes.Current.config
        
    let name = Setting.simple data.Name

    do
        this.Content(
            column()
            |+ PrettySetting("themes.edittheme.themename", TextEntry(name, "none")).Pos(200.0f)
            |+ PrettyButton("themes.edittheme.gameplay", fun () -> Menu.ShowPage EditGameplayConfigPage).Pos(300.0f)
        )

    override this.Title = data.Name
    override this.OnClose() =
        Themes.Current.changeConfig
            { data with
                Name = name.Value
            }
        refreshThemes()