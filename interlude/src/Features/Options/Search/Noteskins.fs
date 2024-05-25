namespace Interlude.Features.OptionsMenu.Search

open Prelude
open Prelude.Skinning.Noteskins
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Content
open Interlude.Features.Import
open Interlude.Features.LevelSelect
open Interlude.Features.OptionsMenu.Noteskins
open Interlude.Features.EditNoteskin
open Interlude.Features.Gameplay

module Noteskins =

    let search_noteskin_settings (tokens: string array) : SearchResult seq =
        seq {
            if token_match tokens [|%"noteskins"|] then
                yield PageButton(
                    "noteskins",
                    (fun () -> NoteskinsPage().Show())
                )
                , 2, 2, PageWidth.Normal

                if not Content.Noteskin.IsEmbedded then
                    yield PageButton(
                        "noteskins.edit",
                        (fun () -> EditNoteskinPage(false).Show())
                    )
                        .Tooltip(Tooltip.Info("noteskins.edit"))
                    , 2, 2, PageWidth.Normal

                if not (Suggestions.in_endless_mode()) then
                    yield PageButton(
                        "noteskins.get_more",
                        (fun () ->
                            Menu.Exit()

                            if Screen.change Screen.Type.Import Transitions.Flags.Default then
                                ImportScreen.switch_to_noteskins ()
                        )
                    )
                    , 2, 2, PageWidth.Normal

                yield PageButton(
                    "noteskins.open_folder",
                    (fun () -> open_directory (get_game_folder "Noteskins"))
                )
                    .Tooltip(Tooltip.Info("noteskins.open_folder"))
                , 2, 2, PageWidth.Normal

            if token_match tokens [|%"hud"|] || token_match tokens (HUDElement.FULL_LIST |> Seq.map HUDElement.name |> Array.ofSeq) then
                yield PageButton(
                    "hud",
                    (fun () ->
                        if Content.Noteskin.IsEmbedded then
                            EditHUDPage().Show()
                        elif
                            SelectedChart.WITH_COLORS.IsSome
                            && Screen.change_new
                                (fun () -> HUDEditor.edit_hud_screen (SelectedChart.CHART.Value, SelectedChart.WITH_COLORS.Value, ignore))
                                Screen.Type.Practice
                                Transitions.Flags.Default
                        then
                            Menu.Exit()
                    )
                )
                    .Tooltip(Tooltip.Info("hud"))
                , 2, 2, PageWidth.Normal
        }