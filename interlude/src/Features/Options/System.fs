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
                Color = (fun () -> (if this.Selected then Colors.pink_accent elif this.Focused then Colors.yellow_accent else Colors.white), Colors.shadow_1),
                Align = Alignment.LEFT,
                Position = Position.TrimLeft 20.0f)
            |* Clickable((fun () -> if not this.Selected then this.Select()), 
                OnHover = fun b -> if b then this.Focus())

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

            container.Add(PageButton(
                "system.hotkeys.reset",
                (fun () -> ConfirmPage(L"system.hotkeys.reset.confirm", Hotkeys.reset_all).Show()),
                Icon = Icons.reset))

            for hk in Hotkeys.hotkeys.Keys do
                if hk <> "none" then
                    container.Add( PageSetting(sprintf "hotkeys.%s" hk, hotkeyEditor hk).Tooltip(Tooltip.Info(sprintf "hotkeys.%s" hk)) )
            this.Content scrollContainer

        override this.Title = L"system.hotkeys.name"
        override this.OnClose() = ()

    type SystemPage() as this =
        inherit Page()

        do
            this.Content(
                column()
                |+ PageSetting("system.windowmode", Selector.FromEnum config.WindowMode)
                    .Pos(200.0f)
                    .Tooltip(Tooltip.Info("system.windowmode"))
                // todo: way to edit resolution settings?
                |+ PageSetting("system.monitor", Selector(Window.monitors, config.Display))
                    .Pos(660.0f)
                    .Tooltip(Tooltip.Info("system.monitor"))

                |+ PageSetting("system.framelimit", Selector.FromEnum config.FrameLimit)
                    .Pos(270.0f)
                    .Tooltip(Tooltip.Info("system.framelimit"))

                |+ PageSetting("system.audiovolume",
                    Slider<_>.Percent(options.AudioVolume |> Setting.trigger Devices.changeVolume, 0.01f) )
                    .Pos(360.0f)
                    .Tooltip(Tooltip.Info("system.audiovolume"))

                |+ PageSetting("system.audiodevice",
                    Selector(Array.ofSeq(Devices.list()), Setting.trigger Devices.change config.AudioDevice) )
                    .Pos(430.0f, 1700.0f)
                    .Tooltip(Tooltip.Info("system.audiodevice"))

                |+ PageSetting("system.audiooffset",
                        { new Slider<float>(options.AudioOffset, 0.01f)
                            with override this.OnDeselected() = base.OnDeselected(); Song.changeGlobalOffset (float32 options.AudioOffset.Value * 1.0f<ms>) } )
                    .Pos(500.0f)
                    .Tooltip(Tooltip.Info("system.audiooffset"))

                |+ PageSetting("system.visualoffset", Slider<float>(options.VisualOffset, 0.01f))
                    .Pos(590.0f)
                    .Tooltip(Tooltip.Info("system.visualoffset"))
                
                |+ PageButton("system.hotkeys", (fun () -> Menu.ShowPage HotkeysPage))
                    .Pos(760.0f)
                    .Tooltip(Tooltip.Info("system.hotkeys"))
            )

        override this.OnClose() = Window.sync (Window.ApplyConfig config)
        override this.Title = L"system.name"