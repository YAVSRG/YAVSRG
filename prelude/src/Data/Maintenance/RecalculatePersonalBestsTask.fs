namespace Prelude.Data.Maintenance

open Percyqaz.Common
open Prelude.Mods
open Prelude.Gameplay.Rulesets
open Prelude.Data.Imports
open Prelude.Data.UserDatabase
open Prelude.Data.ChartDatabase
open Prelude.Data.Library

type RecalculatePersonalBestsTask(library: Library, rulesets: RulesetInfo array, merge_with_existing: bool) =
    inherit BackgroundTask<unit>()

    override this.Run(ctx: ITaskContext) : Async<unit> =
        if rulesets.Length = 0 then
            async { }
        else

        async {
            let charts = library.Charts.Entries |> Seq.toArray

            for i, chart_meta in Seq.indexed charts do
                let data = library.UserData.GetChartData(chart_meta.Hash)

                if not data.Scores.IsEmpty then
                    match library.Charts.GetChart(chart_meta.Hash) with
                    | Error reason -> Logging.Debug "Couldn't load '%s' for pb processing: %s" chart_meta.Hash reason
                    | Ok chart ->

                    let existing_bests = data.PersonalBests

                    let mutable new_bests =
                        if merge_with_existing then data.PersonalBests else Map.empty

                    for score in data.Scores do
                        let initial_ruleset = rulesets.[0]

                        let score_info =
                            ScoreInfo.CreateFromScore(chart_meta, chart, initial_ruleset, score)

                        if score_info.ModStatus = ModStatus.Ranked then

                            let update_bests_for_ruleset (ruleset_info: RulesetInfo) =
                                score_info.RulesetInfo <- ruleset_info

                                if new_bests.ContainsKey(ruleset_info.Hash) then
                                    let ruleset_bests, _ = Bests.update score_info new_bests.[ruleset_info.Hash]
                                    new_bests <- Map.add ruleset_info.Hash ruleset_bests new_bests
                                else
                                    new_bests <- Map.add ruleset_info.Hash (Bests.create score_info) new_bests

                            for ruleset_info in rulesets do
                                update_bests_for_ruleset(ruleset_info)

                            match NativeRuleset.DetectOsu(chart_meta.Origins) with
                            | Some native_ruleset -> update_bests_for_ruleset(native_ruleset.ToInfo())
                            | None -> ()

                    if new_bests <> existing_bests then
                        data.PersonalBests <- new_bests

                ctx.Progress(i + 1, charts.Length)

            library.UserData.SaveChanges()
        }
