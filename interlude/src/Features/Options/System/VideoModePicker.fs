namespace Interlude.Features.OptionsMenu.System

open Percyqaz.Common
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Interlude.UI

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
            let mode = Monitors.get_current_supported_video_modes() |> Array.last
            mode.Width, mode.Height

        let setting =
            setting
            |> Setting.trigger(fun mode ->
                let gcd = gcd mode.Width mode.Height in aspect_ratio <- mode.Width / gcd, mode.Height / gcd
            )

        let dropdown_wrapper =
            DropdownWrapper(fun d -> Position.BorderB(min d.Height 400.0f).Shrink(Style.PADDING, 0.0f))

        let toggle_resolution_dropdown () =
            dropdown_wrapper.Toggle(fun () ->
                let current_mode = setting.Value
                let ax, ay = aspect_ratio

                Dropdown
                    {
                        Items =
                            Monitors.get_current_supported_video_modes()
                            |> Seq.filter(fun mode ->
                                mode.RefreshRate = current_mode.RefreshRate && (mode.Height * ax = mode.Width * ay)
                                || (mode.Width, mode.Height) = (max_width, max_height)
                            )
                            |> Seq.map(fun mode -> mode, sprintf "%ix%i" mode.Width mode.Height)
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
                            Monitors.get_current_supported_video_modes()
                            |> Seq.filter(fun mode ->
                                mode.Width = current_mode.Width && mode.Height = current_mode.Height
                            )
                            |> Seq.map(fun mode -> mode, sprintf "%ihz" mode.RefreshRate)
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
                            Monitors.get_current_supported_video_modes()
                            |> Seq.map(fun mode ->
                                mode, let r = gcd mode.Width mode.Height in mode.Width / r, mode.Height / r
                            )
                            |> Seq.sortBy(fun (mode, _) -> abs(mode.RefreshRate - current_mode.RefreshRate))
                            |> Seq.sortBy(fun (mode, _) -> abs(mode.Height - current_mode.Height))
                            |> Seq.distinctBy snd
                            |> Seq.map(fun (mode, (ax, ay)) ->
                                mode, sprintf "%ix%i (%i:%i)" mode.Width mode.Height ax ay
                            )
                        ColorFunc = K Colors.text
                        Setting = setting
                    }
            )

        let buttons =
            NavigationContainer
                .Row()
                .WrapNavigation(false)
                .With(
                    Button(
                        (fun () -> let mode = setting.Value in sprintf "%ix%i" mode.Width mode.Height),
                        toggle_resolution_dropdown
                    )
                        .Align(Alignment.LEFT)
                        .Position(Position.GridX(1, 3, Style.PADDING * 2.0f)),

                    Button((fun () -> sprintf "%ihz" setting.Value.RefreshRate), toggle_refresh_rate_dropdown)
                        .Position(Position.GridX(2, 3, Style.PADDING * 2.0f)),

                    Button(
                        (fun () -> sprintf "%i:%i" (fst aspect_ratio) (snd aspect_ratio)),
                        toggle_aspect_ratio_dropdown
                    )
                        .Position(Position.GridX(3, 3, Style.PADDING * 2.0f))
                )

        dropdown_wrapper.OnClose <- fun () -> buttons.Focus(false)

        Container.Create(buttons).With(dropdown_wrapper)
