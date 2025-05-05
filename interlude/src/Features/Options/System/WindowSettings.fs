namespace Interlude.Features.OptionsMenu.SystemSettings

open Percyqaz.Common
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude
open Interlude.Options
open Interlude.UI

[<AutoOpen>]
module Monitors =

    let monitors = WindowThread.get_monitors ()

    let get_current_supported_video_modes () : FullscreenVideoMode array =
        let reported_modes =
            if config.Display.Value >= 0 && config.Display.Value < monitors.Length then
                monitors.[config.Display.Value].DisplayModes
            else [||]

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

    let select_fullscreen_size () : unit =
        try
            let supported_video_modes = get_current_supported_video_modes ()

            if not (Array.contains config.FullscreenVideoMode.Value supported_video_modes) then
                config.FullscreenVideoMode.Set supported_video_modes.[supported_video_modes.Length - 1]
        with err ->
            Logging.Debug "Error setting fullscreen video mode - Possibly invalid display selected\n%O" err

    let window_mode_changed (wm: WindowType) : unit =
        if wm = WindowType.Fullscreen then
            select_fullscreen_size ()

type CustomWindowedResolutionPage(setting: Setting<int * int>) =
    inherit Page()

    let tab = Bind.mk Keys.Tab

    let max_width, max_height =
        let mode = get_current_supported_video_modes() |> Array.last
        mode.Width, mode.Height

    let width = Setting.bounded (min 320 (max_width - 1), max_width) (fst setting.Value)
    let height = Setting.bounded (min 240 (max_height - 1), max_height) (snd setting.Value)

    let width_box = NumberEntry.Create width
    let height_box = NumberEntry.Create height

    member this.SaveChanges() =
        if height.Value * 4 / 3 <= width.Value then
            setting.Set (width.Value, height.Value)

    override this.Content () =
        this.OnClose(this.SaveChanges)

        page_container()
            .With(
                PageSetting(%"system.windowresolution.width", width_box).Pos(0),
                PageSetting(%"system.windowresolution.height", height_box).Pos(2),
                Text(%"system.windowresolution.aspect_ratio_warning")
                    .Color(Colors.text_red)
                    .Align(Alignment.CENTER)
                    .Conditional(fun () -> height.Value * 4 / 3 > width.Value)
                    .Pos(4, 1, PageWidth.Normal),
                PageButton(%"confirm.yes", Menu.Back).Pos(6)
            )

    override this.Update (elapsed_ms, moved) =
        let selected = Selection.get_selected_element() |> Option.map (fun w -> (w :?> Widget).Parent)
        if selected = Some width_box && (tab.Pressed() || (%%"select").Pressed()) then height_box.Select false
        elif selected = Some height_box && (tab.Pressed() || (%%"select").Pressed()) then Menu.Back()

        base.Update(elapsed_ms, moved)

    override this.Title = %"system.windowresolution"

type CustomWindowedOffsetPage(setting: Setting<float32 * float32>) =
    inherit Page()

    let horizontal = Setting.percentf (fst setting.Value)
    let vertical = Setting.percentf (snd setting.Value)

    member this.SaveChanges() = setting.Set (horizontal.Value, vertical.Value)

    override this.Content () =
        this.OnClose(this.SaveChanges)
        page_container()
            .With(
                PageSetting(%"system.windowresolution.offset.horizontal", Slider.Percent(horizontal)).Pos(0),
                PageSetting(%"system.windowresolution.offset.vertical", Slider.Percent(vertical)).Pos(2),
                PageButton(%"system.windowresolution.offset.apply", fun () -> this.OnClose()).Pos(5),
                PageButton(%"confirm.yes", Menu.Back).Pos(7)
            )

    override this.Title = %"system.windowresolution.offset"

type WindowedResolutionPicker =

    static member Create(setting: Setting<WindowedResolution>) : Container =

        let dropdown_wrapper = DropdownWrapper(fun d -> Position.SliceT(d.Height + 60.0f |> min 600.0f).ShrinkT(60.0f).Shrink(Style.PADDING, 0.0f))
        let res_setting = Setting.make (fun v -> setting.Set (v, snd setting.Value)) (setting.Get >> fst)
        let offset_setting = Setting.make (fun v -> setting.Set (fst setting.Value, v)) (setting.Get >> snd)

        let toggle_resolution_dropdown () =
            dropdown_wrapper.Toggle(fun () ->
                Dropdown
                    {
                        Items =
                            let max_res = get_current_supported_video_modes() |> Array.last
                            Seq.concat [
                                [|(0, 0), %"system.windowresolution.custom"|]
                                WindowedResolution.PRESETS
                                |> Array.filter (fun (w, h) -> w <= max_res.Width && h <= max_res.Height)
                                |> Array.map (fun (w, h) -> (w, h), sprintf "%ix%i" w h)
                            ]
                        ColorFunc = K Colors.text
                        Setting =
                            { res_setting with
                                Set = fun v ->
                                    if v = (0, 0) then
                                        CustomWindowedResolutionPage(res_setting).Show()
                                    else res_setting.Set v
                            }
                    }
            )

        let buttons =
            NavigationContainer.Row()
                .WrapNavigation(false)
                .With(
                    Button(
                        (fun () -> let w, h = res_setting.Value in sprintf "%ix%i" w h),
                        toggle_resolution_dropdown
                    )
                        .Align(Alignment.LEFT)
                        .Position(Position.GridX(1, 2, Style.PADDING * 2.0f)),

                    Button(
                        (fun () -> if offset_setting.Value = (0.5f, 0.5f) then %"system.windowresolution.offset.center" else %"system.windowresolution.offset.custom"),
                        (fun () -> CustomWindowedOffsetPage(offset_setting).Show())
                    )
                        .Position(Position.GridX(2, 2, Style.PADDING * 2.0f))
                )

        dropdown_wrapper.OnClose <- fun () -> buttons.Focus false
        Container.Create(buttons).With(dropdown_wrapper)

type VideoModePicker =

    static member Create(setting: Setting<FullscreenVideoMode>) : Container =

        let rec gcd a b =
            if a = b then a
            elif a > b then gcd (a - b) b
            else gcd a (b - a)

        let mutable aspect_ratio =
            let mode = setting.Value
            let gcd = gcd mode.Width mode.Height
            mode.Width / gcd, mode.Height / gcd

        let max_width, max_height =
            let mode = get_current_supported_video_modes() |> Array.last
            mode.Width, mode.Height

        let setting = setting |> Setting.trigger (fun mode -> let gcd = gcd mode.Width mode.Height in aspect_ratio <- mode.Width / gcd, mode.Height / gcd)

        let dropdown_wrapper = DropdownWrapper(fun d -> Position.BorderB(min d.Height 400.0f).Shrink(Style.PADDING, 0.0f))

        let toggle_resolution_dropdown () =
            dropdown_wrapper.Toggle(fun () ->
                let current_mode = setting.Value
                let (ax, ay) = aspect_ratio
                Dropdown
                    {
                        Items =
                            get_current_supported_video_modes ()
                            |> Seq.filter (fun mode -> mode.RefreshRate = current_mode.RefreshRate && (mode.Height * ax = mode.Width * ay) || (mode.Width, mode.Height) = (max_width, max_height))
                            |> Seq.map (fun mode ->
                                mode, sprintf "%ix%i" mode.Width mode.Height
                            )
                        ColorFunc = K Colors.text
                        Setting = setting
                    }
            )

        let toggle_refresh_rate_dropdown () =
            dropdown_wrapper.Toggle(fun () ->
                let current_mode = setting.Value
                Dropdown
                    {
                        Items =
                            get_current_supported_video_modes ()
                            |> Seq.filter (fun mode -> mode.Width = current_mode.Width && mode.Height = current_mode.Height)
                            |> Seq.map (fun mode ->
                                mode, sprintf "%ihz" mode.RefreshRate
                            )
                        ColorFunc = K Colors.text
                        Setting = setting
                    }
            )

        let toggle_aspect_ratio_dropdown () =
            dropdown_wrapper.Toggle(fun () ->
                let current_mode = setting.Value
                Dropdown
                    {
                        Items =
                            get_current_supported_video_modes ()
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

        let buttons =
            NavigationContainer.Row()
                .WrapNavigation(false)
                .With(
                    Button(
                        (fun () -> let mode = setting.Value in sprintf "%ix%i" mode.Width mode.Height),
                        toggle_resolution_dropdown
                    )
                        .Align(Alignment.LEFT)
                        .Position(Position.GridX(1, 3, Style.PADDING * 2.0f)),

                    Button(
                        (fun () -> sprintf "%ihz" setting.Value.RefreshRate),
                        toggle_refresh_rate_dropdown
                    )
                        .Position(Position.GridX(2, 3, Style.PADDING * 2.0f)),

                    Button(
                        (fun () -> sprintf "%i:%i" (fst aspect_ratio) (snd aspect_ratio)),
                        toggle_aspect_ratio_dropdown
                    )
                        .Position(Position.GridX(3, 3, Style.PADDING * 2.0f))
                )

        dropdown_wrapper.OnClose <- fun () -> buttons.Focus(false)

        Container.Create(buttons).With(dropdown_wrapper)