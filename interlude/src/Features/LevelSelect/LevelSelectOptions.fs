namespace Interlude.Features.LevelSelect

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Interlude.Options
open Interlude.UI
open Interlude.Features.Collections
open Interlude.Features.Tables

type LevelSelectOptionsPage() =
    inherit Page()

    override this.Content() =
        this.OnClose(LevelSelect.refresh_all)

        page_container()
            .With(
                PageSetting(
                    %"levelselect.only_show_grades",
                    Checkbox options.TreeShowGradesOnly
                )
                    .Help(Help.Info("levelselect.only_show_grades"))
                    .Pos(0),
                PageSetting(
                    %"levelselect.personal_best_timestamps",
                    Checkbox options.TreeShowTimestamps
                )
                    .Pos(2),
                PageSetting(
                    %"levelselect.always_show_collections",
                    Checkbox options.TreeAlwaysShowCollections
                )
                    .Help(Help.Info("levelselect.always_show_collections"))
                    .Pos(4),
                PageSetting(
                    %"levelselect.show_native_text",
                    Checkbox options.TreeShowNativeText
                )
                    .Help(Help.Info("levelselect.show_native_text"))
                    .Pos(6),
                PageSetting(
                    %"levelselect.only_suggest_new_songs",
                    Checkbox options.SuggestionsOnlyNew
                )
                    .Help(Help.Info("levelselect.only_suggest_new_songs"))
                    .Pos(9),
                PageSetting(
                    %"levelselect.enable_rate_suggestions",
                    Checkbox options.SuggestionsEnableRates
                )
                    .Help(Help.Info("levelselect.enable_rate_suggestions"))
                    .Pos(11),
                PageSetting(
                    %"levelselect.min_suggestion_rate",
                    Slider (options.SuggestionsMinRate |> Setting.trigger (fun v -> options.SuggestionsMaxRate |> Setting.app (max v)) |> Setting.uom)
                )
                    .Help(Help.Info("levelselect.min_suggestion_rate"))
                    .Conditional(options.SuggestionsEnableRates.Get)
                    .Pos(13),
                PageSetting(
                    %"levelselect.max_suggestion_rate",
                    Slider (options.SuggestionsMaxRate |> Setting.trigger (fun v -> options.SuggestionsMinRate |> Setting.app (min v)) |> Setting.uom)
                )
                    .Help(Help.Info("levelselect.max_suggestion_rate"))
                    .Conditional(options.SuggestionsEnableRates.Get)
                    .Pos(15),
                PageButton(
                    %"library.tables",
                    fun () -> SelectTablePage(LevelSelect.refresh_all).Show()
                )
                    .Hotkey("table")
                    .Pos(17),
                PageButton(
                    %"library.collections",
                    fun () -> ManageCollectionsPage().Show()
                )
                    .Hotkey("collections")
                    .Pos(19)
            )

    override this.Title = Icons.SETTINGS + " " + %"levelselect.options"