namespace Interlude.Features.LevelSelect

open Percyqaz.Flux.UI
open Prelude.Charts
open Prelude
open Prelude.Data.Library.Endless
open Prelude.Data.Library.Sorting
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Content
open Interlude.Features.Gameplay

type EndlessModeMenu(info: Chart.LoadedChartInfo) as this =
    inherit Page()

    let start() =

        Endless.begin_endless_mode
        <| EndlessModeState.create
            {
                BaseDifficulty = info.Rating.Physical
                BaseChart = info.CacheInfo, rate.Value
                Filter = LevelSelect.filter |> Filter.except_keywords
                Mods = selected_mods.Value
                RulesetId = Rulesets.current_hash
                Ruleset = Rulesets.current
                Library = Content.Library
                ScoreDatabase = Content.Scores
                Priority = Endless.priority.Value
            }
        if not (LevelSelect.try_play info) then
            Endless.exit_endless_mode()
        Menu.Exit()

    do
        page_container()
        |+ PageButton.Once("levelselect.endless_mode.start", start)
            .Pos(3)
        |+ PageSetting("levelselect.endless_mode.priority", 
            Selector<_>(
                [|
                    SuggestionPriority.Variety, %"levelselect.endless_mode.priority.variety"
                    SuggestionPriority.Consistency, %"levelselect.endless_mode.priority.consistency"
                |],
                Endless.priority
            )
        )
            .Pos(0)
        |>> Container
        |> this.Content

    override this.Title = %"levelselect.endless_mode.name"
    override this.OnClose() = ()