namespace Interlude.Features.OptionsMenu.SystemSettings

open Percyqaz.Common
open Percyqaz.Flux.Windowing
open Interlude.Options

module Monitors =

    let monitors = WindowThread.get_monitors()

    let get_current_supported_video_modes () : FullscreenVideoMode array =
        let reported_modes =
            if config.Display.Value >= 0 && config.Display.Value < monitors.Length then
                monitors.[config.Display.Value].DisplayModes
            else
                [||]

        if reported_modes.Length = 0 then [| { Width = 1920; Height = 1080; RefreshRate = 60 } |] else reported_modes

    let select_fullscreen_size () : unit =
        try
            let supported_video_modes = get_current_supported_video_modes()

            if not(Array.contains config.FullscreenVideoMode.Value supported_video_modes) then
                config.FullscreenVideoMode.Set(supported_video_modes.[supported_video_modes.Length - 1])
        with err ->
            Logging.Debug "Error setting fullscreen video mode - Possibly invalid display selected\n%O" err

    let window_mode_changed (wm: WindowType) : unit =
        if wm = WindowType.Fullscreen then
            select_fullscreen_size()
