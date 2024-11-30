namespace Interlude.Features.OptionsMenu.SystemSettings

open Percyqaz.Common
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Prelude
open Interlude.Options
open Interlude.UI

type WindowedResolution(setting: Setting<int * int>) as this =
    inherit Container(NodeType.Button(fun () -> this.ToggleDropdown()))

    let dropdown_wrapper = DropdownWrapper(fun d -> Position.SliceT(d.Height + 60.0f).ShrinkT(60.0f).Shrink(Style.PADDING, 0.0f))

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
                    Items = WindowResolution.PRESETS |> Seq.map (fun (w, h) -> (w, h), sprintf "%ix%i" w h)
                    ColorFunc = K Colors.text
                    Setting = setting
                }
        )

type VideoMode(setting: Setting<FullscreenVideoMode>, modes_thunk: unit -> FullscreenVideoMode array) as this =
    inherit Container(NodeType.Container(fun () -> Some this.Buttons))

    let rec gcd a b =
        if a = b then a
        elif a > b then gcd (a - b) b
        else gcd a (b - a)

    let mutable aspect_ratio =
        let mode = setting.Value
        let gcd = gcd mode.Width mode.Height
        mode.Width / gcd, mode.Height / gcd

    let setting = setting |> Setting.trigger (fun mode -> let gcd = gcd mode.Width mode.Height in aspect_ratio <- mode.Width / gcd, mode.Height / gcd)

    let buttons = 
        GridFlowContainer(PRETTYHEIGHT - 10.0f, 3)
        |+ Button(
            (fun () -> let mode = setting.Value in sprintf "%ix%i" mode.Width mode.Height),
            (fun () -> this.ToggleResolutionDropdown())
        )
        |+ Button(
            (fun () -> sprintf "%ihz" setting.Value.RefreshRate),
            (fun () -> this.ToggleRefreshRateDropdown())
        )
        |+ Button(
            (fun () -> sprintf "%i:%i" (fst aspect_ratio) (snd aspect_ratio)),
            (fun () -> this.ToggleAspectRatioDropdown())
        )

    let dropdown_wrapper = 
        DropdownWrapper(
            (fun d -> 
                Position.BorderB(min d.Height 400.0f).Shrink(Style.PADDING, 0.0f)
            ),
            OnClose = fun () -> buttons.Focus(false)
        )

    member this.Buttons = buttons

    override this.Init(parent) =
        this
        |+ buttons
        |* dropdown_wrapper

        base.Init parent

    member this.ToggleResolutionDropdown() =
        dropdown_wrapper.Toggle(fun () ->
            let current_mode = setting.Value
            let (ax, ay) = aspect_ratio
            Dropdown
                {
                    Items =
                        modes_thunk ()
                        |> Seq.filter (fun mode -> mode.RefreshRate = current_mode.RefreshRate && mode.Height * ax = mode.Width * ay)
                        |> Seq.map (fun mode ->
                            mode, sprintf "%ix%i" mode.Width mode.Height
                        )
                    ColorFunc = K Colors.text
                    Setting = setting
                }
        )

    member this.ToggleRefreshRateDropdown() =
        dropdown_wrapper.Toggle(fun () ->
            let current_mode = setting.Value
            let (ax, ay) = aspect_ratio
            Dropdown
                {
                    Items =
                        modes_thunk ()
                        |> Seq.filter (fun mode -> mode.Width = current_mode.Width && mode.Height = current_mode.Height)
                        |> Seq.map (fun mode ->
                            mode, sprintf "%ihz" mode.RefreshRate
                        )
                    ColorFunc = K Colors.text
                    Setting = setting
                }
        )

    member this.ToggleAspectRatioDropdown() =
        dropdown_wrapper.Toggle(fun () ->
            let current_mode = setting.Value
            Dropdown
                {
                    Items =
                        modes_thunk ()
                        |> Seq.map (fun mode -> mode, let r = gcd mode.Width mode.Height in mode.Width / r, mode.Height / r)
                        |> Seq.sortBy (fun (mode, _) -> abs (mode.RefreshRate - current_mode.RefreshRate))
                        |> Seq.sortBy (fun (mode, _) -> abs (mode.Height - current_mode.Height))
                        |> Seq.distinctBy snd
                        |> Seq.map (fun (mode, (ax, ay)) ->
                            mode, sprintf "%ix%i (%i:%i)" mode.Width mode.Height ax ay
                        )
                    ColorFunc = K Colors.text
                    Setting = setting
                }
        )

[<AutoOpen>]
module Monitors =

    let monitors = WindowThread.get_monitors ()

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
        // todo: better ui for windowed mode since you can no longer do custom res
        if wm = WindowType.Fullscreen then
            select_fullscreen_size ()

type SystemPage() =
    inherit Page()

    override this.Content() =
        window_mode_changed config.WindowMode.Value

        page_container()
        |+ PageButton(
            %"system.performance",
            (fun () -> PerformanceSettingsPage().Show())
        )
            .Help(Help.Info("system.performance"))
            .Pos(0)

        |+ PageButton(%"system.hotkeys", (fun () -> Menu.ShowPage HotkeysPage))
            .Pos(2)

        |+ PageButton(%"system.audio", fun () -> AudioPage().Show())
            .Pos(4)

        |+ PageSetting(
            %"system.windowmode",
            SelectDropdown(
                [|
                    WindowType.Windowed, %"system.windowmode.windowed"
                    WindowType.Borderless, %"system.windowmode.borderless"
                    WindowType.Fullscreen, %"system.windowmode.fullscreen"
                |],
                config.WindowMode
                |> Setting.trigger window_mode_changed
                |> Setting.trigger (fun _ -> WindowThread.defer (fun () -> WindowThread.apply_config config.ToOptions))
            )
        )
            .Pos(7)
        |+ PageSetting(
            %"system.windowresolution",
            WindowedResolution(config.WindowResolution |> Setting.trigger (fun _ -> WindowThread.defer (fun () -> WindowThread.apply_config config.ToOptions)))
        )
            .Help(Help.Info("system.windowresolution"))
            .Pos(9)
            .Conditional(fun () -> config.WindowMode.Value = WindowType.Windowed)
        |+ PageSetting(
            %"system.monitor",
            SelectDropdown(
                monitors |> Seq.map (fun m -> m.Id, m.FriendlyName) |> Array.ofSeq,
                config.Display 
                |> Setting.trigger (fun _ -> select_fullscreen_size (); WindowThread.defer (fun () -> WindowThread.apply_config config.ToOptions))
            )
        )
            .Pos(9)
            .Conditional(fun () -> config.WindowMode.Value <> WindowType.Windowed)
        |+ PageSetting(
            %"system.videomode",
            VideoMode(
                config.FullscreenVideoMode |> Setting.trigger (fun _ -> WindowThread.defer (fun () -> WindowThread.apply_config config.ToOptions)),
                get_current_supported_video_modes
            )
        )
            .Help(Help.Info("system.videomode"))
            .Pos(11)
            .Conditional(fun () -> config.WindowMode.Value = WindowType.Fullscreen)
        |+ PageSetting(%"system.visualoffset", Slider(Setting.uom options.VisualOffset, Step = 1f))
            .Help(Help.Info("system.visualoffset"))
            .Pos(13)
        :> Widget

    override this.OnClose() = ()

    override this.Title = %"system"