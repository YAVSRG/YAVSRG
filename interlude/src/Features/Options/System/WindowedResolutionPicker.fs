namespace Interlude.Features.OptionsMenu.SystemSettings

open Percyqaz.Common
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Prelude
open Interlude.UI

type WindowedResolutionPicker =

    static member Create(setting: Setting<WindowedResolution>) : Container =

        let dropdown_wrapper =
            DropdownWrapper(fun d ->
                Position.SliceT(d.Height + 60.0f |> min 600.0f).ShrinkT(60.0f).Shrink(Style.PADDING, 0.0f)
            )

        let res_setting =
            Setting.make (fun v -> setting.Set(v, snd setting.Value)) (setting.Get >> fst)

        let offset_setting =
            Setting.make (fun v -> setting.Set(fst setting.Value, v)) (setting.Get >> snd)

        let toggle_resolution_dropdown () =
            dropdown_wrapper.Toggle(fun () ->
                Dropdown
                    {
                        Items =
                            let max_res = Monitors.get_current_supported_video_modes() |> Array.last

                            Seq.concat
                                [
                                    [| (0, 0), %"system.windowresolution.custom" |]
                                    WindowedResolution.PRESETS
                                    |> Array.filter(fun (w, h) -> w <= max_res.Width && h <= max_res.Height)
                                    |> Array.map(fun (w, h) -> (w, h), sprintf "%ix%i" w h)
                                ]
                        ColorFunc = K Colors.text
                        Setting =
                            { res_setting with
                                Set =
                                    fun v ->
                                        if v = (0, 0) then
                                            CustomWindowedResolutionPage(res_setting).Show()
                                        else
                                            res_setting.Set(v)
                            }
                    }
            )

        let buttons =
            NavigationContainer
                .Row()
                .WrapNavigation(false)
                .With(
                    Button((fun () -> let w, h = res_setting.Value in sprintf "%ix%i" w h), toggle_resolution_dropdown)
                        .Align(Alignment.LEFT)
                        .Position(Position.GridX(1, 2, Style.PADDING * 2.0f)),

                    Button(
                        (fun () ->
                            if offset_setting.Value = (0.5f, 0.5f) then
                                %"system.windowresolution.offset.center"
                            else
                                %"system.windowresolution.offset.custom"
                        ),
                        (fun () -> CustomWindowedOffsetPage(offset_setting).Show())
                    )
                        .Position(Position.GridX(2, 2, Style.PADDING * 2.0f))
                )

        dropdown_wrapper.OnClose <- fun () -> buttons.Focus(false)
        Container.Create(buttons).With(dropdown_wrapper)
