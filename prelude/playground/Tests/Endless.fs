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

        let sc_j4 = Rulesets.SC.create 4
        let sc_j4_id = Ruleset.hash sc_j4

        let ctx : LibraryViewContext =
            {
                Library = library
                ScoreDatabase = score_db
                Ruleset = sc_j4
                RulesetId = sc_j4_id
                Rate = 1.0f
            }

        let search = "icyworld sa'eed"
        let search_result = LibraryView.get_groups (FilterParts.parse search |> Filter.FromParts) Grouping.modes.["pack"] Sorting.modes.["difficulty"] ctx
        let start = search_result.Values |> Seq.head |> fun group -> fst group.Charts.[0]

        printfn "Starting with %s - %s [%s] by %s" start.Artist start.Title start.DifficultyName start.Creator

        let state = EndlessModeState.create()
        let mutable suggestion_ctx = 
            {
                Library = library
                ScoreDatabase = score_db
                BaseChart = start, 1.0f
                MinimumRate = 1.0f
                MaximumRate = 1.5f
                OnlyNewCharts = false
                Filter = Filter.Empty
                Ruleset = sc_j4
                RulesetId = sc_j4_id
                Mods = Map.empty
            }

        let mutable loop = true
        while loop do
            match EndlessModeState.next suggestion_ctx state with
            | None -> Logging.Info("Nothing found :("); loop <- false
            | Some next ->
                suggestion_ctx <- next.NextContext
                printfn "Next chart: %s - %s [%s] by %s ON RATE %.2f" next.Chart.Artist next.Chart.Title next.Chart.DifficultyName next.Chart.Creator next.Rate

                printfn ""
                match Cache.load next.Chart library.Cache with
                | Ok chart ->
                    printfn "This is classed as: %A [%.2f]" (library.Cache.Patterns.[next.Chart.Hash].Category) (DifficultyRating.calculate 1.0f chart.Notes |> _.Physical)
                | Error reason -> ()
        