namespace Interlude.Features.OptionsMenu.Search

open Prelude
open Prelude.Skinning.HudLayouts
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Content
open Interlude.Features.HUD.Edit
open Interlude.Features.Noteskins.Edit
open Interlude.Features.Noteskins.Browser
open Interlude.Features.Noteskins
open Interlude.Features.Gameplay
open Interlude.Features.Import.osu

module Noteskins =

    let search_noteskin_settings (tokens: string array) : SearchResult seq =
        results {
            if token_match tokens [|%"noteskins"|] then
                yield PageButton(
                    %"noteskins",
                    (fun () -> SelectNoteskinsPage().Show())
                )

                if not Content.Noteskin.IsEmbedded then
                    yield PageButton(
                        %"noteskins.edit",
                        (fun () -> EditNoteskinPage(false).Show())
                    )
                        .Tooltip(Tooltip.Info("noteskins.edit"))

                yield PageButton(
                    %"noteskins.get_more",
                    (fun () -> NoteskinsBrowserPage().Show())
                )

                yield PageButton(
                    %"noteskins.open_folder",
                    (fun () -> open_directory (get_game_folder "Noteskins"))
                )
                    .Tooltip(Tooltip.Info("noteskins.open_folder"))

            if token_match tokens [|%"noteskins"; %"noteskins.import_from_osu"|] then
                yield PageButton(
                    %"noteskins.import_from_osu",
                    (fun () -> Skins.OsuSkinsListPage().Show())
                )

            if token_match tokens [|%"hud"|] || token_match tokens (HudElement.FULL_LIST |> Seq.map HudElement.name |> Array.ofSeq) then
                yield PageButton(
                    %"hud",
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
                    .Tooltip(Tooltip.Info("hud"))

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