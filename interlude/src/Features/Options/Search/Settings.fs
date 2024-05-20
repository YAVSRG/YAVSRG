namespace Interlude.Features.OptionsMenu.Search

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Windowing
open Prelude
open Prelude.Data.Library.Caching
open Interlude.Content
open Interlude.Options
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Features.OptionsMenu.SystemSettings
open Interlude.Features.OptionsMenu.Gameplay
open Interlude.Features.Pacemaker
open Interlude.Features

module Settings =

    let search_system_settings (tokens: string array) : SearchResult seq =
        seq {
            if token_match tokens [|%"system.performance"|] then
                yield PageButton(
                    "system.performance",
                    (fun () -> PerformanceSettingsPage().Show())
                )
                    .Tooltip(Tooltip.Info("system.performance"))
                , 2, 2, PageWidth.Normal
            if token_match tokens [|%"system.windowmode"; %"system.windowresolution"; %"system.monitor"; %"system.videomode"|] then
                yield! [
                    PageSetting(
                        "system.windowmode",
                        SelectDropdown.FromEnum(
                            config.WindowMode
                            |> Setting.trigger window_mode_changed
                            |> Setting.trigger (fun _ -> Window.defer (Window.ApplyConfig config))
                        )
                    )
                        .Tooltip(Tooltip.Info("system.windowmode")) :> Widget
                    , 2, 2, PageWidth.Normal
                    Conditional(
                        (fun () -> config.WindowMode.Value = WindowType.Windowed),
                        PageSetting(
                            "system.windowresolution",
                            WindowedResolution(config.WindowResolution |> Setting.trigger (fun _ -> Window.defer (Window.ApplyConfig config)))
                        )
                            .Tooltip(Tooltip.Info("system.windowresolution"))
                    )
                    , 2, 0, PageWidth.Normal
                    Conditional(
                        (fun () -> config.WindowMode.Value <> WindowType.Windowed),
                        PageSetting(
                            "system.monitor",
                            SelectDropdown(
                                monitors |> Seq.map (fun m -> m.Id, m.FriendlyName) |> Array.ofSeq,
                                config.Display 
                                |> Setting.trigger (fun _ -> select_fullscreen_size (); Window.defer (Window.ApplyConfig config))
                            )
                        )
                            .Tooltip(Tooltip.Info("system.monitor"))
                    )
                    , 2, 2, PageWidth.Normal
                    Conditional(
                        (fun () -> config.WindowMode.Value = WindowType.Fullscreen),
                        PageSetting(
                            "system.videomode",
                            VideoMode(
                                config.FullscreenVideoMode |> Setting.trigger (fun _ -> Window.defer (Window.ApplyConfig config)),
                                get_current_supported_video_modes
                            )
                        )
                            .Tooltip(Tooltip.Info("system.videomode"))
                    )
                    , 2, 2, PageWidth.Normal
                ]
            if token_match tokens [|%"system.audiovolume"|] then
                yield PageSetting(
                    "system.audiovolume",
                    Slider.Percent(
                        options.AudioVolume
                        |> Setting.trigger (fun v -> Devices.change_volume (v, v))
                        |> Setting.f32
                    )
                )
                    .Tooltip(Tooltip.Info("system.audiovolume"))
                , 2, 2, PageWidth.Normal
            if token_match tokens [|%"system.audiodevice"|] then
                yield PageSetting(
                    "system.audiodevice",
                    SelectDropdown(Array.ofSeq (Devices.list ()), Setting.trigger Devices.change config.AudioDevice)
                )
                    .Tooltip(Tooltip.Info("system.audiodevice"))
                , 2, 2, PageWidth.Normal
                
            if token_match tokens [|%"system.audiooffset"|] then
                yield PageSetting(
                    "system.audiooffset",
                    { new Slider(options.AudioOffset, Step = 1f) with
                        override this.OnDeselected(by_mouse: bool) =
                            base.OnDeselected by_mouse
                            Song.set_global_offset (options.AudioOffset.Value * 1.0f<ms>)
                    }
                )
                    .Tooltip(Tooltip.Info("system.audiooffset"))
                , 2, 2, PageWidth.Normal
            
            if token_match tokens [|%"system.visualoffset"|] then
                yield PageSetting("system.visualoffset", Slider(options.VisualOffset, Step = 1f))
                    .Tooltip(Tooltip.Info("system.visualoffset"))
                , 2, 2, PageWidth.Normal
                
            if token_match tokens [|%"system.hotkeys"; %"gameplay.keybinds"|] then
                yield PageButton("system.hotkeys", (fun () -> Menu.ShowPage HotkeysPage))
                    .Tooltip(Tooltip.Info("system.hotkeys"))
                , 2, 2, PageWidth.Normal
        }

    let search_gameplay_settings (tokens: string array) : SearchResult seq =
        seq {
            if token_match tokens [|%"gameplay.scrollspeed"|] then
                yield PageSetting("gameplay.scrollspeed", Slider.Percent(options.ScrollSpeed))
                    .Tooltip(Tooltip.Info("gameplay.scrollspeed"))
                , 2, 2, PageWidth.Normal
                yield Text(
                    (fun () ->
                        [
                            (options.ScrollSpeed.Value * 31.0f / 2.38f).ToString("F1")
                            (options.ScrollSpeed.Value * 33.9f / 2.38f).ToString("F1")
                            "C" + (60000.0f * options.ScrollSpeed.Value / Interlude.Content.Content.NoteskinConfig.ColumnWidth).ToString("F0")
                        ]
                        %> "gameplay.scrollspeed.info"
                    ),
                    Align = Alignment.CENTER
                ), 1, 1, PageWidth.Normal
            if token_match tokens [|%"gameplay.hitposition"|] then
                yield PageSetting("gameplay.hitposition", Slider(options.HitPosition, Step = 1f))
                    .Tooltip(Tooltip.Info("gameplay.hitposition"))
                , 2, 2, PageWidth.Normal
            if token_match tokens [|%"gameplay.upscroll"|] then
                yield PageSetting("gameplay.upscroll", Checkbox options.Upscroll)
                    .Tooltip(Tooltip.Info("gameplay.upscroll"))
                , 2, 2, PageWidth.Normal
            if token_match tokens [|%"gameplay.backgrounddim"|] then
                yield PageSetting("gameplay.backgrounddim", Slider.Percent(options.BackgroundDim))
                    .Tooltip(Tooltip.Info("gameplay.backgrounddim"))
                , 2, 2, PageWidth.Normal
            if token_match tokens [|%"gameplay.lanecover"|] then
                yield PageButton("gameplay.lanecover", (fun () -> Menu.ShowPage LanecoverPage))
                    .Tooltip(Tooltip.Info("gameplay.lanecover"))
                , 2, 2, PageWidth.Normal
            if token_match tokens [|%"gameplay.pacemaker"|] then
                yield PageButton("gameplay.pacemaker", (fun () -> Menu.ShowPage PacemakerOptionsPage))
                    .Tooltip(Tooltip.Info("gameplay.pacemaker").Body(%"gameplay.pacemaker.hint"))
                , 2, 2, PageWidth.Normal
            if token_match tokens [|%"system.hotkeys"; %"gameplay.keybinds"|] then
                let keymode: Setting<Keymode> = Setting.simple <| Gameplay.Chart.keymode ()

                let binds = GameplayKeybinder(keymode)
                yield PageSetting(
                    "generic.keymode",
                    Selector.FromEnum(keymode |> Setting.trigger (ignore >> binds.OnKeymodeChanged))
                )
                , 2, 2, PageWidth.Normal
                yield PageSetting("gameplay.keybinds", binds)
                    .Tooltip(Tooltip.Info("gameplay.keybinds"))
                    .Pos(21, 2, PageWidth.Full)
                , 2, 2, PageWidth.Normal
        }

    let search_advanced_settings (tokens: string array) : SearchResult seq =
        seq {
            if token_match tokens [|%"advanced.enableconsole"|] then
                yield PageSetting("advanced.enableconsole", Checkbox options.EnableConsole)
                , 2, 2, PageWidth.Normal
            if token_match tokens [|%"advanced.confirmexit"|] then
                yield PageSetting("advanced.confirmexit", Checkbox options.ConfirmExit)
                    .Tooltip(Tooltip.Info("advanced.confirmexit"))
                , 2, 2, PageWidth.Normal
            if token_match tokens [|%"advanced.holdtogiveup"|] then
                yield PageSetting("advanced.holdtogiveup", Checkbox options.HoldToGiveUp)
                    .Tooltip(Tooltip.Info("advanced.holdtogiveup"))
                , 2, 2, PageWidth.Normal
            if token_match tokens [|%"advanced.vanishingnotes"|] then
                yield PageSetting("advanced.vanishingnotes", Checkbox options.VanishingNotes)
                    .Tooltip(Tooltip.Info("advanced.vanishingnotes"))
                , 2, 2, PageWidth.Normal
            if token_match tokens [|%"advanced.autocalibrateoffset"|] then
                yield PageSetting("advanced.autocalibrateoffset", Checkbox options.AutoCalibrateOffset)
                    .Tooltip(Tooltip.Info("advanced.autocalibrateoffset"))
                , 2, 2, PageWidth.Normal
            if token_match tokens [|%"advanced.buildpatterncache"|] then
                yield PageButton.Once("advanced.buildpatterncache",
                    fun () ->
                        Cache.cache_patterns.Request(
                            (Content.Cache, true),
                            fun () ->
                                Notifications.system_feedback (
                                    Icons.ALERT_OCTAGON,
                                    %"notification.pattern_cache_complete.title",
                                    ""
                                )
                        )

                        Notifications.system_feedback (
                            Icons.ALERT_OCTAGON,
                            %"notification.pattern_cache_started.title",
                            %"notification.pattern_cache_started.body"
                        )
                )
                    .Tooltip(Tooltip.Info("advanced.buildpatterncache"))
                , 2, 2, PageWidth.Normal
            if token_match tokens [|%"advanced.advancedrecommendations"|] then
                yield PageSetting("advanced.advancedrecommendations", Checkbox options.AdvancedRecommendations)
                    .Tooltip(Tooltip.Info("advanced.advancedrecommendations"))
                , 2, 2, PageWidth.Normal
        }