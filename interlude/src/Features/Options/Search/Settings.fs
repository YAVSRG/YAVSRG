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
open Interlude.Features.Rulesets
open Interlude.Features.Collections
open Interlude.Features.Tables
open Interlude.Features.Tables.Browser
open Interlude.Features.LevelSelect
open Interlude.Features.Import.osu
open Interlude.Features.Import.Etterna

module Settings =

    let search_system_settings (tokens: string array) : SearchResult seq =
        results {
            if token_match tokens [|%"system.performance"; %"search_keywords.performance"|] then
                yield PageButton(
                    %"system.performance",
                    (fun () -> PerformanceSettingsPage().Show())
                )
                    .Tooltip(Tooltip.Info("system.performance"))
            if token_match tokens [|%"system.windowmode"; %"system.windowresolution"; %"system.monitor"; %"system.videomode"; %"system.windowmode.windowed"; %"system.windowmode.borderless"; %"system.windowmode.fullscreen"; %"search_keywords.monitor"|] then
                yield PageSetting(
                    %"system.windowmode",
                    SelectDropdown(
                        [|
                            WindowType.Windowed, %"system.windowmode.windowed"
                            WindowType.Borderless, %"system.windowmode.borderless"
                            WindowType.Fullscreen, %"system.windowmode.fullscreen"
                        |],
                        config.WindowMode
                        |> Setting.trigger window_mode_changed
                        |> Setting.trigger (fun _ -> Window.defer (Window.ApplyConfig config))
                    )
                )
                    .Tooltip(Tooltip.Info("system.windowmode")) :> Widget
                yield PageSetting(
                    %"system.windowresolution",
                    WindowedResolution(config.WindowResolution |> Setting.trigger (fun _ -> Window.defer (Window.ApplyConfig config)))
                )
                    .Tooltip(Tooltip.Info("system.windowresolution"))
                    .Conditional(fun () -> config.WindowMode.Value = WindowType.Windowed)
                , 2, 0, PageWidth.Normal
                yield PageSetting(
                    %"system.monitor",
                    SelectDropdown(
                        monitors |> Seq.map (fun m -> m.Id, m.FriendlyName) |> Array.ofSeq,
                        config.Display 
                        |> Setting.trigger (fun _ -> select_fullscreen_size (); Window.defer (Window.ApplyConfig config))
                    )
                )
                    .Tooltip(Tooltip.Info("system.monitor"))
                    .Conditional(fun () -> config.WindowMode.Value <> WindowType.Windowed)
                yield PageSetting(
                    %"system.videomode",
                    VideoMode(
                        config.FullscreenVideoMode |> Setting.trigger (fun _ -> Window.defer (Window.ApplyConfig config)),
                        get_current_supported_video_modes
                    )
                )
                    .Tooltip(Tooltip.Info("system.videomode"))
                    .Conditional(fun () -> config.WindowMode.Value = WindowType.Fullscreen)
            if token_match tokens [|%"system.audiovolume"|] then
                yield PageSetting(
                    %"system.audiovolume",
                    Slider.Percent(
                        options.AudioVolume
                        |> Setting.trigger (fun v -> Devices.change_volume (v, v))
                        |> Setting.f32
                    )
                )
                    .Tooltip(Tooltip.Info("system.audiovolume"))
            if token_match tokens [|%"system.audiodevice"|] then
                yield PageSetting(
                    %"system.audiodevice",
                    SelectDropdown(Array.ofSeq (Devices.list ()), Setting.trigger Devices.change config.AudioDevice)
                )
                    .Tooltip(Tooltip.Info("system.audiodevice"))
                
            if token_match tokens [|%"system.audiooffset"|] then
                yield PageSetting(
                    %"system.audiooffset",
                    { new Slider(options.AudioOffset, Step = 1f) with
                        override this.OnDeselected(by_mouse: bool) =
                            base.OnDeselected by_mouse
                            Song.set_global_offset (options.AudioOffset.Value * 1.0f<ms>)
                    }
                )
                    .Tooltip(Tooltip.Info("system.audiooffset"))
            
            if token_match tokens [|%"system.visualoffset"|] then
                yield PageSetting(%"system.visualoffset", Slider(options.VisualOffset, Step = 1f))
                    .Tooltip(Tooltip.Info("system.visualoffset"))
                
            if token_match tokens [|%"system.hotkeys"; %"gameplay.keybinds"|] then
                yield PageButton(%"system.hotkeys", (fun () -> Menu.ShowPage HotkeysPage))
                    .Tooltip(Tooltip.Info("system.hotkeys"))

            if token_match tokens [|%"system.automatic_offset"|] then
                yield PageSetting(%"system.automatic_offset", Checkbox options.AutoCalibrateOffset)
                    .Tooltip(Tooltip.Info("system.automatic_offset"))

            if token_match tokens [|%"system.confirm_exit"|] then
                yield PageSetting(%"system.confirm_exit", Checkbox options.ConfirmExit)
                    .Tooltip(Tooltip.Info("system.confirm_exit"))

            if token_match tokens [|%"system.enable_console"|] then
                yield PageSetting(%"system.enable_console", Checkbox options.EnableConsole)
        }

    let search_gameplay_settings (tokens: string array) : SearchResult seq =
        results {
            if token_match tokens [|%"gameplay.scrollspeed"|] then
                yield PageSetting(%"gameplay.scrollspeed", Slider.Percent(options.ScrollSpeed))
                    .Tooltip(Tooltip.Info("gameplay.scrollspeed"))
                yield Text(
                    (fun () ->
                        [
                            ((1080.0f - options.HitPosition.Value) / options.ScrollSpeed.Value).ToString("F0")
                            (options.ScrollSpeed.Value * 31.0f / 2.38f).ToString("F1")
                            (options.ScrollSpeed.Value * 33.9f / 2.38f).ToString("F1")
                            "C" + (60000.0f * options.ScrollSpeed.Value / Content.NoteskinConfig.ColumnWidth).ToString("F0")
                        ]
                        %> "gameplay.scrollspeed.info"
                    ),
                    Align = Alignment.CENTER
                ), 1, 1, PageWidth.Custom (PRETTYTEXTWIDTH + PRETTYWIDTH)
            if token_match tokens [|%"gameplay.hitposition"|] then
                yield PageSetting(%"gameplay.hitposition", Slider(options.HitPosition, Step = 1f))
                    .Tooltip(Tooltip.Info("gameplay.hitposition"))
            if token_match tokens [|%"gameplay.upscroll"|] then
                yield PageSetting(%"gameplay.upscroll", Checkbox options.Upscroll)
                    .Tooltip(Tooltip.Info("gameplay.upscroll"))
            if token_match tokens [|%"gameplay.backgrounddim"|] then
                yield PageSetting(%"gameplay.backgrounddim", Slider.Percent(options.BackgroundDim))
                    .Tooltip(Tooltip.Info("gameplay.backgrounddim"))
            if token_match tokens [|%"gameplay.lanecover"|] then
                yield PageButton(%"gameplay.lanecover", (fun () -> LanecoverPage().Show()))
                    .Tooltip(Tooltip.Info("gameplay.lanecover"))
            if token_match tokens [|%"gameplay.pacemaker"|] then
                yield PageButton(%"gameplay.pacemaker", (fun () -> PacemakerOptionsPage().Show()))
            if token_match tokens [|%"rulesets"|] then
                yield PageButton(%"rulesets", (fun () -> SelectRulesetPage().Show()))
                    .Tooltip(Tooltip.Info("rulesets"))
            if token_match tokens [|%"rulesets.add"|] then
                yield PageButton(%"rulesets.add", (fun () -> AddRulesetsPage().Show()))
            if token_match tokens [|%"system.hotkeys"; %"gameplay.keybinds"; %"search_keywords.binds"|] then
                yield PageSetting(%"gameplay.keybinds", GameplayKeybinder.KeymodeAndKeybinder())
                    .Tooltip(Tooltip.Info("gameplay.keybinds"))
                , 2, 2, PageWidth.Full
            
            if token_match tokens [|%"gameplay.hold_to_give_up"|] then
                yield PageSetting(%"gameplay.hold_to_give_up", Checkbox options.HoldToGiveUp)
                    .Tooltip(Tooltip.Info("gameplay.hold_to_give_up"))

            if token_match tokens [|%"gameplay.hide_hit_notes"|] then
                yield PageSetting(%"gameplay.hide_hit_notes", Checkbox options.VanishingNotes)
                    .Tooltip(Tooltip.Info("gameplay.hide_hit_notes"))
        }

    let search_library_settings (tokens: string array) : SearchResult seq =
        results {
            if token_match tokens [|%"library.collections"|] then
                yield PageButton(
                    %"library.collections",
                    (fun () -> ManageCollectionsPage().Show()),
                    Icon = Icons.FOLDER
                )
                    .Tooltip(Tooltip.Info("library.collections"))
            if token_match tokens [|%"library.tables"|] then
                yield PageButton(
                    %"library.tables",
                    (fun () -> SelectTablePage(LevelSelect.refresh_all).Show()),
                    Icon = Icons.SIDEBAR
                )
                    .Tooltip(Tooltip.Info("library.tables"))
            if token_match tokens [|%"tables.browser"|] then
                yield PageButton(
                    %"tables.browser",
                    (fun () -> TableBrowserPage().Show())
                )
            if token_match tokens [|%"etterna_pack_browser"|] then
                yield PageButton(
                    %"etterna_pack_browser",
                    fun () -> EtternaPacksBrowserPage().Show()
                )
            if token_match tokens [|%"beatmap_browser"|] then
                yield PageButton(
                    %"beatmap_browser",
                    fun () -> BeatmapBrowserPage().Show()
                )
            if token_match tokens [|%"library.recache_charts"|] then
                yield PageButton.Once(
                    %"library.recache_charts",
                    fun () ->
                        Cache.recache_service.Request(
                            Content.Cache,
                            fun () ->
                                Notifications.task_feedback (Icons.FOLDER, %"notification.recache_complete", "")
                        )

                        Notifications.action_feedback (Icons.FOLDER, %"notification.recache", "")
                )
                    .Tooltip(Tooltip.Info("library.recache_charts"))
            if token_match tokens [|%"library.recache_patterns"|] then
                yield PageButton.Once(
                    %"library.recache_patterns",
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
                    .Tooltip(Tooltip.Info("library.recache_patterns"))
        }