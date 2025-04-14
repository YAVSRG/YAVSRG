namespace Interlude.Features.OptionsMenu.Search

open Percyqaz.Common
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Skins.HudLayouts
open Interlude.Options
open Interlude.Content
open Interlude.UI
open Interlude.Features.Skins
open Interlude.Features.OptionsMenu.SystemSettings
open Interlude.Features.OptionsMenu.Gameplay
open Interlude.Features.OptionsMenu.Library

module SearchResults =

    let private results (tokens: string array) : SearchResult seq =
        results {
            if token_match tokens [|%"system.performance"; %"system.cpu_saver"; %"system.msaa"; %"system.performance.antijitter"; %"search_keywords.performance"; |] then
                yield SystemPage.Performance()

            if token_match tokens [|%"system.windowmode"; %"system.windowresolution"; %"system.monitor"; %"system.videomode"; %"system.windowmode.windowed"; %"system.windowmode.borderless"; %"system.windowmode.fullscreen"; %"search_keywords.monitor"|] then
                yield SystemPage.WindowMode()
                yield SystemPage.WindowedResolution(), 2, 0, PageWidth.Normal
                yield SystemPage.Monitor()
                yield SystemPage.VideoMode(), 2, 0, PageWidth.Normal
                yield SystemPage.LetterboxResolution(), 2, 3, PageWidth.Normal

            if token_match tokens [|%"system.audiovolume"|] then
                yield AudioPage.AudioVolume()
            if token_match tokens [|%"system.audiodevice"|] then
                yield AudioPage.AudioDevice()

            if token_match tokens [|%"system.audiooffset"|] then
                yield AudioPage.AudioOffset()
            if token_match tokens [|%"system.visualoffset"|] then
                yield SystemPage.VisualOffset()
            if token_match tokens [|%"system.automatic_offset"|] then
                yield AudioPage.AutomaticOffset()

            if token_match tokens [|%"system.audio_pitch_rates"|] then
                yield AudioPage.RatesChangePitch()
            if token_match tokens [|%"system.menus_muffle_song"|] then
                yield AudioPage.MenusMuffleSong(), 2, 3, PageWidth.Normal

            if token_match tokens [|%"system.hotkeys"; %"gameplay.keybinds"|] then
                yield SystemPage.Hotkeys()
            if token_match tokens [|%"system.hotkeys"; %"gameplay.keybinds"; %"search_keywords.binds"|] then
                yield GameplayPage.Keybinds(), 2, 3, PageWidth.Normal

            if token_match tokens [|%"gameplay.scrollspeed"|] then
                yield GameplayPage.ScrollSpeed(), 2, 3, PageWidth.Normal
            if token_match tokens [|%"gameplay.hitposition"|] then
                yield GameplayPage.HitPosition()
            if token_match tokens [|%"gameplay.upscroll"|] then
                yield GameplayPage.Upscroll()
            if token_match tokens [|%"gameplay.backgrounddim"|] then
                yield GameplayPage.BackgroundDim()
            if token_match tokens [|%"gameplay.lanecover"; %"gameplay.lanecover.hidden"; %"gameplay.lanecover.sudden"|] then
                yield GameplayPage.Lanecover()
            if token_match tokens [|%"gameplay.pacemaker"; %"gameplay.pacemaker.fail_mid_song"; %"gameplay.pacemaker.onlysavenewrecords"; %"gameplay.pacemaker.save_failed_scores"|] then
                yield GameplayPage.Pacemaker()
            if token_match tokens [|%"gameplay.hold_to_give_up"|] then
                yield GameplayPage.HoldToGiveUp()
            if token_match tokens [|%"gameplay.hide_hit_notes"|] then
                yield GameplayPage.HideHitNotes()
            if token_match tokens [|%"gameplay.on_quit_out"|] then
                yield GameplayPage.OnQuitOut()
            if token_match tokens [|%"rulesets"|] then
                yield LibraryPage.ManageRulesets(), 2, 3, PageWidth.Normal

            if token_match tokens [|%"skins"|] then
                yield PageButton(%"skins", fun () -> SelectSkinsPage().Show())
                yield PageButton(%"noteskin.edit", SkinActions.edit_or_extract_noteskin)
                yield ImportsPage.GetSkins()
            if token_match tokens [|%"skins"; %"skins.import_from_osu"|] then
                yield ImportsPage.ImportOsuSkins()
            if token_match tokens [|%"hud.edit"|] || token_match tokens (HudElement.FULL_LIST |> Seq.map HudElement.name |> Array.ofSeq) then
                yield PageButton(%"hud.edit", fun () -> SkinActions.edit_hud ignore)
                    .Help(Help.Info("hud.edit")), 2, 3, PageWidth.Normal

            if token_match tokens [|%"etterna_pack_browser"; %"menu.import"|] then
                yield ImportsPage.GetEtternaPacks()
            if token_match tokens [|%"beatmap_browser"; %"menu.import"|] then
                yield ImportsPage.GetOsuSongs()
            if token_match tokens [|%"library.collections"|] then
                yield LibraryPage.ManageCollections()
            if token_match tokens [|%"levelselect.options"|] then
                yield LibraryPage.LevelSelectOptions()
            if token_match tokens [|%"tables.browser"; %"menu.import"|] then
                yield ImportsPage.GetTables()
            if token_match tokens [|%"library.tables"|] then
                yield LibraryPage.ManageTables(), 2, 3, PageWidth.Normal

            if token_match tokens [|%"library.recache_patterns"|] then
                yield LibraryPage.RecachePatterns()
            if token_match tokens [|%"library.recalculate_personal_bests"|] then
                yield LibraryPage.RecalculateScores()
            if token_match tokens [|%"library.vacuum"|] then
                yield LibraryPage.Vacuum()

            // Secret, search-only results

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
                ), 2, 3, PageWidth.Normal

            if token_match tokens [|%"themes.theme"; %"themes.showthemesfolder"|] then
                yield PageSetting(%"themes.theme", SelectDropdown(Themes.list (), options.Theme))
                yield PageButton(%"themes.showthemesfolder", fun () -> open_directory (get_game_folder "Themes"))
        }

    let get (query: string) : Widget = search results query