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
            |+ Text(icon)
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
            ) ).Pos(200.0f)
        |+ PrettySetting(
            "themes.edittheme.gameplay.generic.float",
            Selector<_>.FromBool(
                Setting.make (fun v -> setting.Set { setting.Value with Float = v } ) (fun () -> setting.Value.Float)
            ) ).Pos(300.0f)

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
            } ).Pos(380.0f)
        
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
            } ).Pos(460.0f)

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
            } ).Pos(540.0f)
        
        |+ PrettyButton(
            "themes.edittheme.gameplay.generic.reset",
            fun () -> setting.Value <- { default_pos with Enabled = setting.Value.Enabled }
           ).Pos(620.0f)

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
                Selector<_>.FromBool grade_colors ).Pos(720.0f)
            |+ PrettySetting(
                "themes.edittheme.gameplay.accuracymeter.showname",
                Selector<_>.FromBool show_name ).Pos(800.0f)
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

type EditGameplayConfigPage() as this =
    inherit Page()

    do
        this.Content(
            column()
            |+ PrettyButton("themes.edittheme.gameplay.accuracymeter", fun () -> Menu.ShowPage EditAccuracyMeterPage).Pos(200.0f)
            |+ PrettyButton("themes.edittheme.gameplay.hitmeter", ignore).Pos(280.0f)
            |+ PrettyButton("themes.edittheme.gameplay.lifemeter", ignore).Pos(360.0f)
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