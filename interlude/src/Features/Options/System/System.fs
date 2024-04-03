namespace Interlude.Features.OptionsMenu.System

open Percyqaz.Common
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Prelude
open Interlude.Options
open Interlude.UI
open Interlude.UI.Menu

type private WindowedResolution(setting: Setting<int * int>) as this =
    inherit Container(NodeType.Button(fun () -> this.ToggleDropdown()))

    override this.Init(parent) =
        this
        |+ Text((fun () -> let w, h = setting.Value in sprintf "%ix%i" w h), Align = Alignment.LEFT)
        |* Clickable.Focus this

        base.Init parent

    member this.ToggleDropdown() =
        match this.Dropdown with
        | Some _ -> this.Dropdown <- None
        | _ ->
            let d =
                Dropdown
                    {
                        Items = WindowResolution.presets |> Seq.map (fun (w, h) -> (w, h), sprintf "%ix%i" w h)
                        ColorFunc = K Colors.text
                        OnClose = fun () -> this.Dropdown <- None
                        Setting = setting
                    }

            d.Position <- Position.SliceTop(d.Height + 60.0f).TrimTop(60.0f).Margin(Style.PADDING, 0.0f)
            d.Init this
            this.Dropdown <- Some d

    member val Dropdown: Dropdown<int * int> option = None with get, set

    override this.Draw() =
        base.Draw()

        match this.Dropdown with
        | Some d -> d.Draw()
        | None -> ()

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        match this.Dropdown with
        | Some d -> d.Update(elapsed_ms, moved)
        | None -> ()

type private VideoMode(setting: Setting<FullscreenVideoMode>, modes_thunk: unit -> FullscreenVideoMode array) as this =
    inherit Container(NodeType.Button(fun () -> this.ToggleDropdown()))

    override this.Init(parent) =
        this
        |+ Text(
            (fun () -> let mode = setting.Value in sprintf "%ix%i@%ihz" mode.Width mode.Height mode.RefreshRate),
            Align = Alignment.LEFT
        )
        |* Clickable.Focus this

        base.Init parent

    member this.ToggleDropdown() =
        match this.Dropdown with
        | Some _ -> this.Dropdown <- None
        | _ ->
            let d =
                Dropdown
                    {
                        Items =
                            modes_thunk ()
                            |> Seq.map (fun mode ->
                                mode, sprintf "%ix%i@%ihz" mode.Width mode.Height mode.RefreshRate
                            )
                        ColorFunc = K Colors.text
                        OnClose = fun () -> this.Dropdown <- None
                        Setting = setting
                    }

            d.Position <- Position.SliceTop(560.0f).TrimTop(60.0f).Margin(Style.PADDING, 0.0f)
            d.Init this
            this.Dropdown <- Some d

    member val Dropdown: Dropdown<FullscreenVideoMode> option = None with get, set

    override this.Draw() =
        base.Draw()

        match this.Dropdown with
        | Some d -> d.Draw()
        | None -> ()

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        match this.Dropdown with
        | Some d -> d.Update(elapsed_ms, moved)
        | None -> ()

type SystemPage() as this =
    inherit Page()

    let mutable has_changed = false
    let mark_changed = fun (_: 'T) -> has_changed <- true

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

    let pick_suitable_video_mode () =
        try
            let supported_video_modes = get_current_supported_video_modes ()

            if not (Array.contains config.FullscreenVideoMode.Value supported_video_modes) then
                config.FullscreenVideoMode.Set supported_video_modes.[supported_video_modes.Length - 1]
        with err ->
            Logging.Debug("Error setting fullscreen video mode - Possibly invalid display selected", err)

    let monitor_select =
        PageSetting(
            "system.monitor",
            Selector(
                monitors |> Seq.map (fun m -> m.Id, m.FriendlyName) |> Array.ofSeq,
                config.Display
                |> Setting.trigger (fun _ ->
                    mark_changed ()
                    pick_suitable_video_mode ()
                )
            )
        )
            .Pos(5)
            .Tooltip(Tooltip.Info("system.monitor"))

    let windowed_resolution_select =
        PageSetting(
            "system.windowresolution",
            WindowedResolution(config.WindowResolution |> Setting.trigger mark_changed)
        )
            .Pos(5)
            .Tooltip(Tooltip.Info("system.windowresolution"))

    let resolution_or_monitor = SwapContainer()

    let window_mode_change (wm) =
        if wm = WindowType.Windowed then
            resolution_or_monitor.Current <- windowed_resolution_select
            Window.sync (Window.EnableResize config.WindowResolution.Set)
        else
            resolution_or_monitor.Current <- monitor_select
            Window.sync (Window.DisableResize)

        if wm = WindowType.Fullscreen then
            pick_suitable_video_mode ()

    do
        window_mode_change (config.WindowMode.Value)

        this.Content(
            page_container()
            |+ PageSetting(
                "system.audiovolume",
                Slider.Percent(
                    options.AudioVolume
                    |> Setting.trigger (fun v -> Devices.change_volume (v, v))
                    |> Setting.f32
                )
            )
                .Pos(10)
                .Tooltip(Tooltip.Info("system.audiovolume"))

            |+ PageSetting(
                "system.audiodevice",
                Selector(Array.ofSeq (Devices.list ()), Setting.trigger Devices.change config.AudioDevice)
            )
                .Pos(12, 2, PageWidth.Full)
                .Tooltip(Tooltip.Info("system.audiodevice"))

            |+ PageSetting(
                "system.audiooffset",
                { new Slider(options.AudioOffset, Step = 1f) with
                    override this.OnDeselected(by_mouse: bool) =
                        base.OnDeselected by_mouse
                        Song.set_global_offset (options.AudioOffset.Value * 1.0f<ms>)
                }
            )
                .Pos(14)
                .Tooltip(Tooltip.Info("system.audiooffset"))

            |+ PageSetting("system.visualoffset", Slider(options.VisualOffset, Step = 1f))
                .Pos(17)
                .Tooltip(Tooltip.Info("system.visualoffset"))

            |+ PageButton("system.hotkeys", (fun () -> Menu.ShowPage HotkeysPage))
                .Pos(19)
                .Tooltip(Tooltip.Info("system.hotkeys"))

            |+ PageButton(
                "system.performance",
                (fun () -> PerformanceSettingsPage().Show())
            )
                .Pos(0)
                .Tooltip(Tooltip.Info("system.performance"))
            |+ Conditional(
                (fun () -> config.RenderMode.Value = FrameLimit.Unlimited),
                Text(%"system.framelimit.unlimited_warning", 
                    Color = K Colors.text_red,
                    Position = pretty_pos(2, 1, PageWidth.Full).TrimLeft(PRETTYTEXTWIDTH),
                    Align = Alignment.LEFT
                )
            )
            |+ PageSetting(
                "system.windowmode",
                Selector.FromEnum(
                    config.WindowMode
                    |> Setting.trigger window_mode_change
                    |> Setting.trigger mark_changed
                )
            )
                .Pos(3)
                .Tooltip(Tooltip.Info("system.windowmode"))
            |+ resolution_or_monitor
            |+ Conditional(
                (fun () -> config.WindowMode.Value = WindowType.Fullscreen),
                PageSetting(
                    "system.videomode",
                    VideoMode(
                        config.FullscreenVideoMode |> Setting.trigger mark_changed,
                        get_current_supported_video_modes
                    )
                )
                    .Pos(7)
                    .Tooltip(Tooltip.Info("system.videomode"))
            )
        )

        this.Add(
            Conditional(
                (fun () -> has_changed),
                Callout.frame
                    (Callout.Small.Icon(Icons.AIRPLAY).Title(%"system.window_changes_hint"))
                    (fun (w, h) -> Position.SliceTop(h).SliceRight(w).Translate(-20.0f, 20.0f))
            )
        )

    override this.OnClose() =
        Window.sync (Window.DisableResize)
        Window.sync (Window.ApplyConfig config)

    override this.Title = %"system.name"
