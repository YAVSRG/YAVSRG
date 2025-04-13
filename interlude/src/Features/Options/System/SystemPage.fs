namespace Interlude.Features.OptionsMenu.SystemSettings

open Percyqaz.Common
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Prelude
open Interlude.Options
open Interlude.UI

type SystemPage() =
    inherit Page()

    static member WindowMode() : PageSetting =
        PageSetting(
            %"system.windowmode",
            SelectDropdown(
                [|
                    WindowType.Windowed, %"system.windowmode.windowed"
                    WindowType.Borderless, %"system.windowmode.borderless"
                    WindowType.BorderlessNoTaskbar, %"system.windowmode.fullscreen_borderless"
                    WindowType.Fullscreen, %"system.windowmode.fullscreen"
                    WindowType.FullscreenLetterbox, %"system.windowmode.fullscreen_letterbox"
                |],
                config.WindowMode
                |> Setting.trigger window_mode_changed
                |> Setting.trigger (ignore >> config.Apply)
            )
        )

    static member WindowedResolution() : Conditional<PageSetting> =
        PageSetting(
            %"system.windowresolution",
            WindowedResolutionPicker.Create(config.WindowedResolution |> Setting.trigger (ignore >> config.Apply))
        )
            .Conditional(fun () -> config.WindowMode.Value = WindowType.Windowed)

    static member Monitor() : Conditional<PageSetting> =
        PageSetting(
            %"system.monitor",
            SelectDropdown(
                monitors |> Seq.map (fun m -> m.Id, m.FriendlyName) |> Array.ofSeq,
                config.Display
                |> Setting.trigger (fun _ -> select_fullscreen_size (); config.Apply())
            )
        )
            .Conditional(fun () -> config.WindowMode.Value <> WindowType.Windowed)

    static member VideoMode() : Conditional<PageSetting> =
        PageSetting(
            %"system.videomode",
            VideoModePicker.Create(
                config.FullscreenVideoMode
                |> Setting.trigger (ignore >> config.Apply)
            )
        )
            .Help(Help.Info("system.videomode"))
            .Conditional(fun () -> config.WindowMode.Value = WindowType.Fullscreen)

    static member LetterboxResolution() : Conditional<PageSetting> =
        PageSetting(
            %"system.letterbox_resolution",
            WindowedResolutionPicker.Create(config.WindowedResolution |> Setting.trigger (ignore >> config.Apply))
        )
            .Conditional(fun () -> config.WindowMode.Value = WindowType.FullscreenLetterbox)

    static member Performance() : PageButton =
        PageButton(
            %"system.performance",
            (fun () -> PerformanceSettingsPage().Show())
        )

    static member Hotkeys() : PageButton =
        PageButton(%"system.hotkeys", (fun () -> HotkeysPage().Show()))

    static member VisualOffset() : PageSetting =
        PageSetting(%"system.visualoffset", Slider(Setting.uom options.VisualOffset, Step = 1f))
            .Help(Help.Info("system.visualoffset"))

    override this.Content() =
        window_mode_changed config.WindowMode.Value

        page_container()
            .With(
                SystemPage.WindowMode().Pos(0),
                SystemPage.WindowedResolution().Pos(2),
                SystemPage.Monitor().Pos(2),
                SystemPage.VideoMode().Pos(4),
                SystemPage.LetterboxResolution().Pos(4),

                SystemPage.Performance().Pos(7),
                SystemPage.Hotkeys().Pos(9),

                PageButton(%"system.audio", fun () -> AudioPage().Show()).Pos(12),
                SystemPage.VisualOffset().Pos(14)
            )

    override this.OnClose() = ()

    override this.Title = %"system"