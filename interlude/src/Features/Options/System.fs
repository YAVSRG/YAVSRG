namespace Interlude.Features.OptionsMenu

open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Prelude.Common
open Interlude.Options
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Menu

module System =

    type Keybinder(hotkey: Hotkey) as this =
        inherit StaticContainer(NodeType.Leaf)

        let set = fun v -> Hotkeys.set hotkey v

        let rec inputCallback(b) =
            match b with
            | Key (k, (ctrl, _, shift)) ->
                set <| Key (k, (ctrl, false, shift))
                this.Focus()
            | _ -> Input.grabNextEvent inputCallback

        do
            this
            |+ Text((fun () -> (!|hotkey).ToString()),
                Color = (fun () -> (if this.Selected then Style.color(255, 1.0f, 0.0f) else Color.White), Color.Black),
                Align = Alignment.LEFT,
                Position = Position.TrimLeft 20.0f)
            |* Clickable((fun () -> if not this.Selected then this.Select()), 
                OnHover = fun b -> if b then this.Focus())
    
        override this.Draw() =
            if this.Selected then Draw.rect this.Bounds (!*Palette.SELECTED)
            elif this.Focused then Draw.rect this.Bounds (!*Palette.HOVER)
            base.Draw()

        override this.OnSelected() =
            base.OnSelected()
            Input.grabNextEvent inputCallback

        override this.OnDeselected() =
            base.OnDeselected()
            Input.removeInputMethod()

    type HotkeysPage() as this =
        inherit Page()

        do
            let hotkeyEditor hk =
                SwitchContainer.Row<Widget>()
                |+ Keybinder(hk, Position = Position.TrimRight PRETTYHEIGHT)
                |+ Button(Icons.reset, (fun () -> Hotkeys.reset hk), Position = Position.SliceRight PRETTYHEIGHT)

            let container = FlowContainer.Vertical<Widget>(PRETTYHEIGHT)
            let scrollContainer = ScrollContainer.Flow(container, Position = Position.Margin(100.0f, 200.0f))

            container.Add(PrettyButton(
                "system.hotkeys.reset",
                (fun () -> ConfirmPage(L"options.system.hotkeys.reset.confirm", Hotkeys.reset_all).Show()),
                Icon = Icons.reset))

            for hk in Hotkeys.hotkeys.Keys do
                if hk <> "none" then
                    container.Add( PrettySetting("hotkeys." + hk, hotkeyEditor hk).Tooltip(Tooltip.Info(sprintf "options.hotkeys.%s" hk)) )
            this.Content scrollContainer

        override this.Title = L"options.system.hotkeys.name"
        override this.OnClose() = ()

    type SystemPage() as this =
        inherit Page()

        do
            this.Content(
                column()
                |+ PrettySetting("system.windowmode", Selector.FromEnum config.WindowMode)
                    .Pos(200.0f)
                    .Tooltip(Tooltip.Info("options.system.windowmode"))

                |+ PrettySetting("system.framelimit", Selector.FromEnum config.FrameLimit)
                    .Pos(270.0f)
                    .Tooltip(Tooltip.Info("options.system.framelimit"))

                |+ PrettySetting("system.audiovolume",
                    Slider<_>.Percent(options.AudioVolume |> Setting.trigger Devices.changeVolume, 0.01f) )
                    .Pos(360.0f)
                    .Tooltip(Tooltip.Info("options.system.audiovolume"))

                |+ PrettySetting("system.audiodevice",
                    Selector(Array.ofSeq(Devices.list()), Setting.trigger Devices.change config.AudioDevice) )
                    .Pos(430.0f, 1700.0f)
                    .Tooltip(Tooltip.Info("options.system.audiodevice"))

                |+ PrettySetting("system.audiooffset",
                        { new Slider<float>(options.AudioOffset, 0.01f)
                            with override this.OnDeselected() = base.OnDeselected(); Song.changeGlobalOffset (float32 options.AudioOffset.Value * 1.0f<ms>) } )
                    .Pos(500.0f)
                    .Tooltip(Tooltip.Info("options.system.audiooffset"))

                |+ PrettySetting("system.visualoffset", Slider<float>(options.VisualOffset, 0.01f))
                    .Pos(590.0f)
                    .Tooltip(Tooltip.Info("options.system.visualoffset"))
                // todo: way to edit resolution settings?
                |+ PrettySetting("system.monitor", Selector(Window.monitors, config.Display))
                    .Pos(660.0f)
                    .Tooltip(Tooltip.Info("options.system.monitor"))
                
                |+ PrettyButton("system.hotkeys", (fun () -> Menu.ShowPage HotkeysPage))
                    .Pos(760.0f)
                    .Tooltip(Tooltip.Info("options.system.hotkeys"))
            )

        override this.OnClose() = Window.apply_config <- Some config
        override this.Title = L"options.system.name"