namespace Interlude.Features.OptionsMenu.Library

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Interlude.Options
open Interlude.UI

type LevelSelectPage() =
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
            %"levelselect.only_suggest_new_songs",
            Checkbox options.SuggestionsOnlyNew
        )
            .Help(Help.Info("levelselect.only_suggest_new_songs"))
            .Pos(3)
        |+ PageSetting(
            %"levelselect.min_suggestion_rate",
            Slider (options.SuggestionsMinRate |> Setting.trigger (fun v -> options.SuggestionsMaxRate |> Setting.app (max v)))
        )
            .Help(Help.Info("levelselect.min_suggestion_rate"))
            .Pos(5)
        |+ PageSetting(
            %"levelselect.min_suggestion_rate",
            Slider (options.SuggestionsMaxRate |> Setting.trigger (fun v -> options.SuggestionsMinRate |> Setting.app (min v)))
        )
            .Help(Help.Info("levelselect.min_suggestion_rate"))
            .Pos(7)
        :> Widget

    override this.OnClose() = ()

    override this.Title = %"levelselect.options"