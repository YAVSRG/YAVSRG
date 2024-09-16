namespace Interlude.Features.LevelSelect

open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude
open Interlude.Options
open Interlude.UI
open Interlude.Features.Collections
open Interlude.Features.Tables

type LevelSelectOptionsPage() =
    inherit Page()

    override this.Content() =
        page_container()
        |+ PageSetting(
            %"levelselect.only_show_grades",
            Checkbox options.TreeShowGradesOnly
        )
            .Help(Help.Info("levelselect.only_show_grades"))
            .Pos(0)
        |+ PageSetting(
            %"levelselect.always_show_collections",
            Checkbox options.TreeAlwaysShowCollections
        )
            .Help(Help.Info("levelselect.always_show_collections"))
            .Pos(2)
        |+ PageSetting(
            %"levelselect.show_native_text",
            Checkbox options.TreeShowNativeText
        )
            .Help(Help.Info("levelselect.show_native_text"))
            .Pos(4)
        |+ PageSetting(
            %"levelselect.only_suggest_new_songs",
            Checkbox options.SuggestionsOnlyNew
        )
            .Help(Help.Info("levelselect.only_suggest_new_songs"))
            .Pos(7)
        |+ PageSetting(
            %"levelselect.min_suggestion_rate",
            Slider (options.SuggestionsMinRate |> Setting.trigger (fun v -> options.SuggestionsMaxRate |> Setting.app (max v)))
        )
            .Help(Help.Info("levelselect.min_suggestion_rate"))
            .Pos(9)
        |+ PageSetting(
            %"levelselect.min_suggestion_rate",
            Slider (options.SuggestionsMaxRate |> Setting.trigger (fun v -> options.SuggestionsMinRate |> Setting.app (min v)))
        )
            .Help(Help.Info("levelselect.min_suggestion_rate"))
            .Pos(11)
        |+ PageButton(
            %"library.tables",
            (fun () -> SelectTablePage(LevelSelect.refresh_all).Show()),
            Hotkey = %%"table"
        )
            .Pos(14)
        |+ PageButton(
            %"library.collections",
            (fun () -> ManageCollectionsPage().Show()),
            Hotkey = %%"collections"
        )
            .Pos(16)
        :> Widget

    override this.OnClose() = LevelSelect.refresh_all()

    override this.Title = Icons.SETTINGS + " " + %"levelselect.options"