namespace Interlude.Features.LevelSelect

open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.Library.Endless
open Prelude.Data.Library.Sorting
open Interlude.Content
open Interlude.Features.Gameplay
open Interlude.UI

type EndlessModeMenu(info: LoadedChartInfo) =
    inherit Page()

    let start() =

        //Suggestions.begin_endless_mode
        //<| EndlessModeState.create
        //    {
        //        BaseDifficulty = info.Rating.Physical
        //        BaseChart = info.CacheInfo, SelectedChart.rate.Value
        //        Filter = LevelSelect.filter |> Filter.except_keywords
        //        Mods = SelectedChart.selected_mods.Value
        //        RulesetId = Rulesets.current_hash
        //        Ruleset = Rulesets.current
        //        Library = Content.Library
        //        ScoreDatabase = Content.Scores
        //        Priority = Suggestions.endless_priority.Value
        //    }
        //<| false
        Menu.Exit()

    override this.Content() =
        page_container()
        |+ PageSetting(%"levelselect.endless_mode.priority", 
            SelectDropdown<_>(
                [|
                    SuggestionPriority.Variety, %"levelselect.endless_mode.priority.variety"
                    SuggestionPriority.Consistency, %"levelselect.endless_mode.priority.consistency"
                |],
                LevelSelect.endless_priority
            )
        )
            .Pos(0)
        |+ PageButton.Once(%"levelselect.endless_mode.start", start)
            .Pos(3)
        :> Widget

    override this.Title = %"levelselect.endless_mode"
    override this.OnClose() = ()