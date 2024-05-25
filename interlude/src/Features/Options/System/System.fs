namespace Interlude.Features.OptionsMenu.SystemSettings

open Percyqaz.Common
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Prelude
open Interlude.Options
open Interlude.UI
open Interlude.UI.Menu

type WindowedResolution(setting: Setting<int * int>) as this =
    inherit Container(NodeType.Button(fun () -> this.ToggleDropdown()))

    let dropdown_wrapper = DropdownWrapper(fun d -> Position.SliceTop(d.Height + 60.0f).TrimTop(60.0f).Margin(Style.PADDING, 0.0f))

    override this.Init(parent) =
        this
        |+ Text((fun () -> let w, h = setting.Value in sprintf "%ix%i" w h), Align = Alignment.LEFT)
        |+ Clickable.Focus this
        |* dropdown_wrapper

        base.Init parent

    member this.ToggleDropdown() =
        dropdown_wrapper.Toggle(fun () ->
            Dropdown
                {
                    Items = WindowResolution.presets |> Seq.map (fun (w, h) -> (w, h), sprintf "%ix%i" w h)
                    ColorFunc = K Colors.text
                    Setting = setting
                }
        )

type VideoMode(setting: Setting<FullscreenVideoMode>, modes_thunk: unit -> FullscreenVideoMode array) as this =
    inherit Container(NodeType.Button(fun () -> this.ToggleDropdown()))

    let dropdown_wrapper = DropdownWrapper(fun d -> Position.SliceTop(560.0f).TrimTop(60.0f).Margin(Style.PADDING, 0.0f))

    override this.Init(parent) =
        this
        |+ Text(
            (fun () -> let mode = setting.Value in sprintf "%ix%i@%ihz" mode.Width mode.Height mode.RefreshRate),
            Align = Alignment.LEFT
        )
        |+ Clickable.Focus this
        |* dropdown_wrapper

        base.Init parent

    member this.ToggleDropdown() =
        dropdown_wrapper.Toggle(fun () ->
            Dropdown
                {
                    Items =
                        modes_thunk ()
                        |> Seq.map (fun mode ->
                            mode, sprintf "%ix%i@%ihz" mode.Width mode.Height mode.RefreshRate
                        )
                    ColorFunc = K Colors.text
                    Setting = setting
                }
        )

[<AutoOpen>]
module Monitors =

    let monitors = Window.get_monitors ()

    let get_current_supported_video_modes () =
        let reported_modes = monitors.[config.Display.Value].DisplayModes

        if reported_modes.Length = 0 then
            [|
                {
                    Width = 1920
                    Height = 1080
                    RefreshRate = 60
                }
            |]
        else
            reported_modes

    let select_fullscreen_size () =
        try
            let supported_video_modes = get_current_supported_video_modes ()

            if not (Array.contains config.FullscreenVideoMode.Value supported_video_modes) then
                config.FullscreenVideoMode.Set supported_video_modes.[supported_video_modes.Length - 1]
        with err ->
            Logging.Debug("Error setting fullscreen video mode - Possibly invalid display selected", err)

    let window_mode_changed (wm: WindowType) =
        if wm = WindowType.Windowed then
            Window.defer (Window.EnableResize config.WindowResolution.Set)
        else
            Window.defer (Window.DisableResize)

        if wm = WindowType.Fullscreen then
            select_fullscreen_size ()

type SystemPage() =
    inherit Page()

    let mutable has_changed = false
    let mark_changed = fun (_: 'T) -> has_changed <- true

    override this.Content() =
        window_mode_changed config.WindowMode.Value

        page_container()
        |+ PageButton(
            "system.performance",
            (fun () -> PerformanceSettingsPage().Show())
        )
            .Tooltip(Tooltip.Info("system.performance"))
            .Pos(0)

        |+ PageSetting(
            "system.windowmode",
            SelectDropdown(
                [|
                    WindowType.Windowed, %"system.windowmode.windowed"
                    WindowType.Borderless, %"system.windowmode.borderless"
                    WindowType.Fullscreen, %"system.windowmode.fullscreen"
                |],
                config.WindowMode
                |> Setting.trigger mark_changed
                |> Setting.trigger window_mode_changed
            )
        )
            .Tooltip(Tooltip.Info("system.windowmode"))
            .Pos(3)
        |+ Conditional(
            (fun () -> config.WindowMode.Value = WindowType.Windowed),
            PageSetting(
                "system.windowresolution",
                WindowedResolution(config.WindowResolution |> Setting.trigger mark_changed)
            )
                .Tooltip(Tooltip.Info("system.windowresolution"))
                .Pos(5)
        )
        |+ Conditional(
            (fun () -> config.WindowMode.Value <> WindowType.Windowed),
            PageSetting(
                "system.monitor",
                SelectDropdown(
                    monitors |> Seq.map (fun m -> m.Id, m.FriendlyName) |> Array.ofSeq,
                    config.Display 
                    |> Setting.trigger (fun _ -> select_fullscreen_size (); mark_changed())
                )
            )
                .Tooltip(Tooltip.Info("system.monitor"))
                .Pos(5)
        )
        |+ Conditional(
            (fun () -> config.WindowMode.Value = WindowType.Fullscreen),
            PageSetting(
                "system.videomode",
                VideoMode(
                    config.FullscreenVideoMode |> Setting.trigger mark_changed,
                    get_current_supported_video_modes
                )
            )
                .Tooltip(Tooltip.Info("system.videomode"))
        )
                .Pos(7)
        |+ PageSetting(
            "system.audiovolume",
            Slider.Percent(
                options.AudioVolume
                |> Setting.trigger (fun v -> Devices.change_volume (v, v))
                |> Setting.f32
            )
        )
            .Tooltip(Tooltip.Info("system.audiovolume"))
            .Pos(10)

        |+ PageSetting(
            "system.audiodevice",
            SelectDropdown(Array.ofSeq (Devices.list ()), Setting.trigger Devices.change config.AudioDevice)
        )
            .Tooltip(Tooltip.Info("system.audiodevice"))
            .Pos(12)

        |+ PageSetting(
            "system.audiooffset",
            { new Slider(options.AudioOffset, Step = 1f) with
                override this.OnDeselected(by_mouse: bool) =
                    base.OnDeselected by_mouse
                    Song.set_global_offset (options.AudioOffset.Value * 1.0f<ms>)
            }
        )
            .Tooltip(Tooltip.Info("system.audiooffset"))
            .Pos(14)

        |+ PageSetting("system.visualoffset", Slider(options.VisualOffset, Step = 1f))
            .Tooltip(Tooltip.Info("system.visualoffset"))
            .Pos(17)

        |+ PageButton("system.hotkeys", (fun () -> Menu.ShowPage HotkeysPage))
            .Tooltip(Tooltip.Info("system.hotkeys"))
            .Pos(19)
        |>> Container
        |+ Conditional(
            (fun () -> has_changed),
            Callout.frame
                (Callout.Small.Icon(Icons.AIRPLAY).Title(%"system.window_changes_hint"))
                (fun (w, h) -> Position.SliceTop(h).SliceRight(w).Translate(-20.0f, 20.0f))
        )
        :> Widget

    override this.OnClose() =
        Window.defer (Window.DisableResize)
        if has_changed then Window.defer (Window.ApplyConfig config)

    override this.Title = %"system"