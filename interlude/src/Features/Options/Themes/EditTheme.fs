namespace Interlude.Features.OptionsMenu.Themes

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude.Data.Themes
open Prelude.Data.Themes.WidgetConfig
open Interlude.Content
open Interlude.UI.Menu

[<AutoOpen>]
module private Helpers =

    let configSetting<'T> : Setting<'T> =
        Setting.make Themes.Current.GameplayConfig.set Themes.Current.GameplayConfig.get

    let inline get_default() = (^T : (static member Default: ^T) ())
     
    let positionEditor (setting: Setting<WidgetConfig>) (default_pos: WidgetConfig) =
        column()
        |+ PrettySetting(
            "themes.edittheme.gameplay.generic.enable",
            Selector<_>.FromBool(
                Setting.make (fun v -> setting.Set { setting.Value with Enabled = v } ) (fun () -> setting.Value.Enabled)
            ) ).Pos(200.0f, PRETTYWIDTH, 60.0f)
        |+ PrettySetting(
            "themes.edittheme.gameplay.generic.float",
            Selector<_>.FromBool(
                Setting.make (fun v -> setting.Set { setting.Value with Float = v } ) (fun () -> setting.Value.Float)
            ) ).Pos(280.0f, PRETTYWIDTH, 60.0f)

        |+ PrettySetting(
            "themes.edittheme.gameplay.generic.leftanchor",
            Slider<_>.Percent(
                Setting.make (fun v -> setting.Set { setting.Value with LeftA = v } ) (fun () -> setting.Value.LeftA)
                |> Setting.bound 0.0f 1.0f,
                0.1f
            ) ).Pos(340.0f, PRETTYWIDTH, 60.0f)
        |+ PrettySetting(
            "themes.edittheme.gameplay.generic.leftoffset",
            Slider<float32>(
                Setting.make (fun v -> setting.Set { setting.Value with Left = v } ) (fun () -> setting.Value.Left)
                |> Setting.bound -200.0f 200.0f,
                0.1f
            ) ).Pos(400.0f, PRETTYWIDTH, 60.0f)
        
        |+ PrettySetting(
            "themes.edittheme.gameplay.generic.rightanchor",
            Slider<_>.Percent(
                Setting.make (fun v -> setting.Set { setting.Value with RightA = v } ) (fun () -> setting.Value.RightA)
                |> Setting.bound 0.0f 1.0f,
                0.1f
            ) ).Pos(460.0f, PRETTYWIDTH, 60.0f)
        |+ PrettySetting(
            "themes.edittheme.gameplay.generic.rightoffset",
            Slider<float32>(
                Setting.make (fun v -> setting.Set { setting.Value with Right = v } ) (fun () -> setting.Value.Right)
                |> Setting.bound -200.0f 200.0f,
                0.1f
            ) ).Pos(520.0f, PRETTYWIDTH, 60.0f)
        
        |+ PrettySetting(
            "themes.edittheme.gameplay.generic.topanchor",
            Slider<_>.Percent(
                Setting.make (fun v -> setting.Set { setting.Value with TopA = v } ) (fun () -> setting.Value.TopA)
                |> Setting.bound 0.0f 1.0f,
                0.1f
            ) ).Pos(580.0f, PRETTYWIDTH, 60.0f)
        |+ PrettySetting(
            "themes.edittheme.gameplay.generic.topoffset",
            Slider<float32>(
                Setting.make (fun v -> setting.Set { setting.Value with Top = v } ) (fun () -> setting.Value.Top)
                |> Setting.bound -200.0f 200.0f,
                0.1f
            ) ).Pos(640.0f, PRETTYWIDTH, 60.0f)
        
        |+ PrettySetting(
            "themes.edittheme.gameplay.generic.bottomanchor",
            Slider<_>.Percent(
                Setting.make (fun v -> setting.Set { setting.Value with BottomA = v } ) (fun () -> setting.Value.BottomA)
                |> Setting.bound 0.0f 1.0f,
                0.1f
            ) ).Pos(700.0f, PRETTYWIDTH, 60.0f)
        |+ PrettySetting(
            "themes.edittheme.gameplay.generic.bottomoffset",
            Slider<float32>(
                Setting.make (fun v -> setting.Set { setting.Value with Bottom = v } ) (fun () -> setting.Value.Bottom)
                |> Setting.bound -200.0f 200.0f,
                0.1f
            ) ).Pos(760.0f, PRETTYWIDTH, 60.0f)
        
        |+ PrettyButton(
            "themes.edittheme.gameplay.generic.reset",
            fun () -> setting.Value <- { default_pos with Enabled = setting.Value.Enabled }
           ).Pos(820.0f, 60.0f)

type EditAccuracyMeterPage() as this =
    inherit Page()

    let data = Themes.Current.GameplayConfig.get<AccuracyMeter>()
    let pos = Setting.simple data.Position
    let default_pos = AccuracyMeter.Default.Position

    do
        this.Content(
            positionEditor pos default_pos
        )

    override this.Title = N"themes.edittheme.gameplay.accuracymeter"
    override this.OnClose() = 
        Themes.Current.GameplayConfig.set<AccuracyMeter>
            { data with
                Position = pos.Value
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