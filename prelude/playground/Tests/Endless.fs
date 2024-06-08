namespace Prelude.Test

open System.IO
open Percyqaz.Common
open Percyqaz.Data.Sqlite
open Prelude.Charts.Processing.Difficulty
open Prelude.Gameplay
open Prelude.Data.Library
open Prelude.Data.Library.Caching
open Prelude.Data.Library.Endless
open Prelude.Data

module Endless =

    let test() =
        
        Directory.SetCurrentDirectory("C:/Interlude/dev") // this is my interlude install location
        let library = Library.load()
        let score_db = ScoreDatabase.create true (Database.from_file "Data/scores.db")

        let sc_j4 = PremadeRulesets.SC.create 4
        let sc_j4_id = Ruleset.hash sc_j4

        let ctx : Sorting.LibraryViewContext =
            {
                Library = library
                ScoreDatabase = score_db
                Ruleset = sc_j4
                RulesetId = sc_j4_id
                Rate = 1.0f
            }

        let search = "icyworld sa'eed"
        let search_result = Sorting.get_groups (Sorting.Filter.parse search) Sorting.grouping_modes.["pack"] Sorting.sorting_modes.["difficulty"] ctx
        let start = search_result.Values |> Seq.head |> fun group -> fst group.Charts.[0]

        printfn "Starting with %s - %s [%s] by %s" start.Artist start.Title start.DifficultyName start.Creator

        let mutable endless_mode_state = 
            EndlessModeState.create {
                Library = library
                ScoreDatabase = score_db
                BaseChart = start, 1.0f
                BaseDifficulty = start.Physical
                Filter = []
                Ruleset = sc_j4
                RulesetId = sc_j4_id
                Mods = Map.empty
                Priority = SuggestionPriority.Consistency
            }

        let mutable loop = true
        while loop do
            match EndlessModeState.next endless_mode_state with
            | None -> Logging.Info("Nothing found :("); loop <- false
            | Some next ->
                endless_mode_state <- next.NewState
                printfn "Next chart: %s - %s [%s] by %s ON RATE %.2f" next.Chart.Artist next.Chart.Title next.Chart.DifficultyName next.Chart.Creator next.Rate

                printfn ""
                match Cache.load next.Chart library.Cache with
                | Ok chart ->
                    printfn "This is classed as: %A [%.2f]" (library.Cache.Patterns.[next.Chart.Hash].Category) (DifficultyRating.calculate 1.0f chart.Notes |> _.Physical)
                | Error reason -> ()
        