namespace Interlude.Features.OptionsMenu.Search

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Data.Library
open Interlude.Content
open Interlude.Options
open Interlude.UI
open Interlude.Features.OptionsMenu.SystemSettings
open Interlude.Features.OptionsMenu.Gameplay
open Interlude.Features.OptionsMenu.Library
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
            if token_match tokens [|%"system.performance"; %"system.cpu_saver"; %"system.msaa"; %"system.performance.antijitter"; %"search_keywords.performance"; |] then
                yield PageButton(
                    %"system.performance",
                    (fun () -> PerformanceSettingsPage().Show())
                )
                    .Help(Help.Info("system.performance"))
            if token_match tokens [|%"system.windowmode"; %"system.windowresolution"; %"system.monitor"; %"system.videomode"; %"system.windowmode.windowed"; %"system.windowmode.borderless"; %"system.windowmode.fullscreen"; %"search_keywords.monitor"|] then
                yield PageSetting(
                    %"system.windowmode",
                    SelectDropdown(
                        [|
                            WindowType.Windowed, %"system.windowmode.windowed"
                            WindowType.Borderless, %"system.windowmode.borderless"
                            WindowType.Fullscreen, %"system.windowmode.fullscreen"
                            WindowType.FullscreenLetterbox, %"system.windowmode.fullscreen_letterbox"
                        |],
                        config.WindowMode
                        |> Setting.trigger window_mode_changed
                        |> Setting.trigger (ignore >> config.Apply)
                    )
                )
                yield PageSetting(
                    %"system.windowresolution",
                    WindowResolutionPicker(config.WindowedResolution |> Setting.trigger (ignore >> config.Apply))
                )
                    .Help(Help.Info("system.windowresolution"))
                    .Conditional(fun () -> config.WindowMode.Value = WindowType.Windowed)
                , 2, 0, PageWidth.Normal
                yield PageSetting(
                    %"system.monitor",
                    SelectDropdown(
                        monitors |> Seq.map (fun m -> m.Id, m.FriendlyName) |> Array.ofSeq,
                        config.Display
                        |> Setting.trigger (fun _ -> select_fullscreen_size (); config.Apply())
                    )
                )
                    .Conditional(fun () -> config.WindowMode.Value <> WindowType.Windowed)
                yield PageSetting(
                    %"system.letterbox_resolution",
                    WindowResolutionPicker(config.WindowedResolution |> Setting.trigger (ignore >> config.Apply))
                )
                    .Conditional(fun () -> config.WindowMode.Value = WindowType.FullscreenLetterbox)
                , 2, 0, PageWidth.Normal
                yield PageSetting(
                    %"system.videomode",
                    VideoMode(
                        config.FullscreenVideoMode
                        |> Setting.trigger (ignore >> config.Apply)
                    )
                )
                    .Help(Help.Info("system.videomode"))
                    .Conditional(fun () -> config.WindowMode.Value = WindowType.Fullscreen)
            if token_match tokens [|%"system.audiovolume"|] then
                yield PageSetting(
                    %"system.audiovolume",
                    Slider.Percent(
                        options.AudioVolume
                        |> Setting.trigger (fun v -> Audio.change_volume (v, v))
                        |> Setting.f32
                    )
                )
            if token_match tokens [|%"system.audiodevice"|] then
                yield PageSetting(
                    %"system.audiodevice",
                    SelectDropdown(Audio.list_devices () |> Array.map (fun d -> d.Index, d.ToString()), Setting.trigger Audio.change_device config.AudioDevice)
                )

            if token_match tokens [|%"system.audiooffset"|] then
                yield PageSetting(
                    %"system.audiooffset",
                    { new Slider(Setting.uom options.AudioOffset, Step = 1f) with
                        override this.OnDeselected(by_mouse: bool) =
                            base.OnDeselected by_mouse
                            Song.set_global_offset options.AudioOffset.Value
                    }
                )
                    .Help(Help.Info("system.audiooffset"))

            if token_match tokens [|%"system.audio_pitch_rates"|] then
                yield PageSetting(
                    %"system.audio_pitch_rates",
                    Checkbox(
                        options.AudioPitchRates
                        |> Setting.trigger (fun v -> Song.set_pitch_rates_enabled v)
                    )
                )
                    .Help(Help.Info("system.audio_pitch_rates"))

            if token_match tokens [|%"system.menus_muffle_song"|] then
                yield PageSetting(%"system.menus_muffle_song",
                    Checkbox(
                        options.MenusMuffleSong
                        |> Setting.trigger (fun b -> if b then Song.set_low_pass 1.0f else Song.set_low_pass 0.0f)
                    )
                )
                    .Help(Help.Info("system.menus_muffle_song"))

            if token_match tokens [|%"system.visualoffset"|] then
                yield PageSetting(%"system.visualoffset", Slider(Setting.uom options.VisualOffset, Step = 1f))
                    .Help(Help.Info("system.visualoffset"))

            if token_match tokens [|%"system.hotkeys"; %"gameplay.keybinds"|] then
                yield PageButton(%"system.hotkeys", (fun () -> Menu.ShowPage HotkeysPage))

            if token_match tokens [|%"system.automatic_offset"|] then
                yield PageSetting(%"system.automatic_offset", Checkbox options.AutoCalibrateOffset)
                    .Help(Help.Info("system.automatic_offset"))

            if token_match tokens [|%"system.enable_console"|] then
                yield PageSetting(%"system.enable_console", Checkbox options.EnableConsole)

            if token_match tokens [|%"system.debug_crash"|] then
                yield PageButton(%"system.debug_crash",
                    fun () ->
                        Logging.Debug "%s" (Audio.debug_info())
                        Logging.Debug "%s" (Render.debug_info())
                        WindowThread.defer (fun () ->
                            Logging.Debug "%s" (WindowThread.debug_info())
                            GameThread.defer (fun () ->
                                failwith "Debug crash, on purpose by pressing the debug crash button"
                            )
                        )
                )
        }

    let search_gameplay_settings (tokens: string array) : SearchResult seq =
        results {
            if token_match tokens [|%"gameplay.scrollspeed"|] then
                yield PageSetting(%"gameplay.scrollspeed", Slider.Percent(Setting.uom options.ScrollSpeed))
                    .Help(Help.Info("gameplay.scrollspeed"))
                yield Text(fun () ->
                        [
                            ((1080.0f - options.HitPosition.Value) / float32 options.ScrollSpeed.Value).ToString("F0")
                            (float32 options.ScrollSpeed.Value * 12.698412f).ToString("F1")
                            (float32 options.ScrollSpeed.Value * 33.9f / 2.38f).ToString("F1")
                            "C" + (60000.0f * float32 options.ScrollSpeed.Value / Content.NoteskinConfig.DefaultColumnWidth).ToString("F0")
                        ]
                        %> "gameplay.scrollspeed.info"
                    )
                    .Align(Alignment.CENTER),
                    1, 1, PageWidth.Custom (PAGE_LABEL_WIDTH + PAGE_ITEM_WIDTH)
            if token_match tokens [|%"gameplay.hitposition"|] then
                yield PageSetting(%"gameplay.hitposition", Slider(options.HitPosition, Step = 1f))
                    .Help(Help.Info("gameplay.hitposition"))
            if token_match tokens [|%"gameplay.upscroll"|] then
                yield PageSetting(%"gameplay.upscroll", Checkbox options.Upscroll)
                    .Help(Help.Info("gameplay.upscroll"))
            if token_match tokens [|%"gameplay.backgrounddim"|] then
                yield PageSetting(%"gameplay.backgrounddim", Slider.Percent(options.BackgroundDim))
                    .Help(Help.Info("gameplay.backgrounddim"))
            if token_match tokens [|%"gameplay.lanecover"|] then
                yield PageButton(%"gameplay.lanecover", fun () -> LanecoverPage().Show())
                    .Help(Help.Info("gameplay.lanecover"))
            if token_match tokens [|%"gameplay.pacemaker"; %"gameplay.pacemaker.fail_mid_song"; %"gameplay.pacemaker.onlysavenewrecords"; %"gameplay.pacemaker.save_failed_scores"|] then
                yield PageButton(%"gameplay.pacemaker", fun () -> PacemakerOptionsPage().Show())
            if token_match tokens [|%"rulesets"|] then
                yield PageButton(%"rulesets", fun () -> SelectRulesetPage().Show())
                    .Help(Help.Info("rulesets"))
            if token_match tokens [|%"rulesets.add"|] then
                yield PageButton(%"rulesets.add", fun () -> AddRulesetsPage().Show())
            if token_match tokens [|%"system.hotkeys"; %"gameplay.keybinds"; %"search_keywords.binds"|] then
                yield PageButton(%"gameplay.keybinds", fun () -> GameplayBindsPage().Show())
                    .Help(Help.Info("gameplay.keybinds"))

            if token_match tokens [|%"gameplay.hold_to_give_up"|] then
                yield PageSetting(%"gameplay.hold_to_give_up", Checkbox options.HoldToGiveUp)
                    .Help(Help.Info("gameplay.hold_to_give_up"))

            if token_match tokens [|%"gameplay.hide_hit_notes"|] then
                yield PageSetting(%"gameplay.hide_hit_notes", Checkbox options.VanishingNotes)
                    .Help(Help.Info("gameplay.hide_hit_notes"))
        }

    let search_library_settings (tokens: string array) : SearchResult seq =
        results {
            if token_match tokens [|%"library.collections"|] then
                yield PageButton(
                    %"library.collections",
                    (fun () -> ManageCollectionsPage().Show()),
                    Icon = Icons.FOLDER
                )
                    .Help(Help.Info("library.collections"))
            if token_match tokens [|%"library.tables"|] then
                yield PageButton(
                    %"library.tables",
                    (fun () -> SelectTablePage(LevelSelect.refresh_all).Show()),
                    Icon = Icons.SIDEBAR
                )
                    .Help(Help.Info("library.tables"))
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
            if token_match tokens [|%"library.recache_patterns"|] then
                yield PageButton.Once(
                    %"library.recache_patterns",
                    Library.recalculate_patterns
                )
                    .Help(Help.Info("library.recache_patterns"))
            if token_match tokens [|%"library.recalculate_personal_bests"|] then
                yield PageButton.Once(
                    %"library.recalculate_personal_bests",
                    Library.recalculate_pbs
                )
                    .Help(Help.Info("library.recalculate_personal_bests"))
        }