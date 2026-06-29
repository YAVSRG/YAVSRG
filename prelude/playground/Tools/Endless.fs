module Endless

open System.IO
open Percyqaz.Common
open Percyqaz.Data.Sqlite
open Prelude
open Prelude.Calculator
open Prelude.Gameplay.Rulesets
open Prelude.Data.User
open Prelude.Data.Library
open Prelude.Data.Library.Endless

let test() =

    Directory.SetCurrentDirectory("C:/Interlude/dev") // this is my interlude install location
    let library = Library.load()
    let user_db = UserDatabase.CreateFullyLoaded(Database.from_file("Data/scores.db"))

    let sc_j4 = SC.create 4
    let sc_j4_id = Ruleset.hash sc_j4

    let ctx : LibraryViewContext =
        {
            Library = library
            UserDatabase = user_db
            Ruleset = sc_j4
            RulesetId = sc_j4_id
            Rate = 1.0f<rate>
        }

    let search = "getty jumper"
    let search_result = LibraryView.get_groups (FilterParts.parse search |> FilteredSearch.Build) Grouping.modes.["pack"] false Sorting.modes.["difficulty"] false false None ctx
    let start = search_result |> Seq.head |> snd |> fun group -> fst group.Charts.[0]

    printfn "Starting with %s - %s [%s] by %s" start.Artist start.Title start.DifficultyName start.Creator

    let state = EndlessModeState.create()
    let mutable suggestion_ctx =
        {
            Library = library
            UserDatabase = user_db
            BaseChart = start, 1.0f<rate>
            MinimumRate = 1.0f<rate>
            MaximumRate = 1.5f<rate>
            OnlyNewCharts = false
            Filter = FilteredSearch.Empty
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
            printfn "Next chart: %s - %s [%s] by %s ON RATE %.2f" next.ChartMeta.Artist next.ChartMeta.Title next.
                                                                                                                 ChartMeta
                                                                                                                 .DifficultyName next.
                                                                                                                                     ChartMeta
                                                                                                                                     .Creator next.Rate

            printfn ""
            match library.Charts.GetChart(next.ChartMeta.Hash) with
            | Ok chart ->
                printfn "This is classed as: %A [%.2f]" next.ChartMeta.Patterns.Category (Difficulty.calculate(1.0f<rate>, chart.ToNoteData()).Overall)
            | Error _ -> ()