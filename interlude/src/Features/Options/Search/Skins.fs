namespace Interlude.Features.OptionsMenu.Search

open Prelude
open Prelude.Skinning.HudLayouts
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Content
open Interlude.Features.EditHUD
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
                        (fun () -> EditNoteskinPage(false).Show())
                    )
                        .Tooltip(Tooltip.Info("noteskin.edit"))

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
                                (fun () -> HUDEditor.edit_hud_screen (SelectedChart.CHART.Value, SelectedChart.WITH_COLORS.Value, ignore))
                                Screen.Type.Practice
                                Transitions.Default
                        then
                            Menu.Exit()
                    )
                )
                    .Tooltip(Tooltip.Info("hud.edit"))

            if token_match tokens [|%"themes.theme"; %"themes.showthemesfolder"|] then
                yield PageSetting(%"themes.theme", 
                    SelectDropdown(Themes.list (), Interlude.Options.options.Theme)
                )
                yield PageButton(
                    %"themes.showthemesfolder", 
                    (fun () -> open_directory (get_game_folder "Themes"))
                )
                    .Tooltip(Tooltip.Info("themes.showthemesfolder"))
        }