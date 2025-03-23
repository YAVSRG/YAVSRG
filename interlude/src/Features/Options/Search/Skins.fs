namespace Interlude.Features.OptionsMenu.Search

open Prelude
open Prelude.Skins.HudLayouts
open Interlude.UI
open Interlude.Content
open Interlude.Options
open Interlude.Features.Skins.EditHUD
open Interlude.Features.Skins.EditNoteskin
open Interlude.Features.Skins.Browser
open Interlude.Features.Skins
open Interlude.Features.Gameplay
open Interlude.Features.Import.osu

module Skins =

    let search_skin_settings (tokens: string array) : SearchResult seq =
        results {
            if token_match tokens [|%"skins"|] then
                yield PageButton(
                    %"skins",
                    (fun () -> SelectSkinsPage().Show())
                )

                if not Content.Noteskin.IsEmbedded then
                    yield PageButton(
                        %"noteskin.edit",
                        (fun () -> EditNoteskinPage().Show())
                    )

                yield PageButton(
                    %"skins.browser",
                    (fun () -> SkinsBrowserPage().Show())
                )

            if token_match tokens [|%"skins"; %"skins.import_from_osu"|] then
                yield PageButton(
                    %"skins.import_from_osu",
                    (fun () -> Skins.OsuSkinsListPage().Show())
                )

            if token_match tokens [|%"hud.edit"|] || token_match tokens (HudElement.FULL_LIST |> Seq.map HudElement.name |> Array.ofSeq) then
                yield PageButton(
                    %"hud.edit",
                    (fun () ->
                        if
                            SelectedChart.WITH_COLORS.IsSome
                            && Screen.change_new
                                (fun () -> EditHudScreen.edit_hud_screen (SelectedChart.CHART.Value, SelectedChart.WITH_COLORS.Value, ignore))
                                ScreenType.EditHud
                                Transitions.Default
                        then
                            Menu.Exit()
                    )
                )
                    .Help(Help.Info("hud.edit"))

            if token_match tokens [|%"themes.theme"; %"themes.showthemesfolder"|] then
                yield PageSetting(%"themes.theme",
                    SelectDropdown(Themes.list (), options.Theme)
                )
                yield PageButton(
                    %"themes.showthemesfolder",
                    (fun () -> open_directory (get_game_folder "Themes"))
                )
        }