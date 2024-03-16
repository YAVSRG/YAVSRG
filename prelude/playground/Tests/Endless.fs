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
        let score_db = ScoreDatabase.create true (Database.from_file "scores.db")

        let sc_j4 = PrefabRulesets.SC.create 4
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
                BaseChart = start
                Filter = []
                Ruleset = sc_j4
                RulesetId = sc_j4_id
                Mods = Map.empty
                Rate = 1.0f
            }

        let format_specifics (specifics: (string * int) array) =
            specifics
            |> Seq.map (fun (s, count) -> sprintf "%s (%i)" s count)
            |> String.concat ", "

        let mutable loop = true
        while loop do
            match EndlessModeState.next endless_mode_state with
            | None -> Logging.Info("Nothing found :("); loop <- false
            | Some next ->
                endless_mode_state <- next.NewState
                printfn "Next chart: %s - %s [%s] by %s" next.Chart.Artist next.Chart.Title next.Chart.DifficultyName next.Chart.Creator

                printfn ""

                for p in library.Patterns.[next.Chart.Hash].Patterns do
                    printfn "%.2fs of %iBPM%s %A [Density %.1f-%.1f] %s" 
                        (p.Amount / 1000.0f<ms>)
                        p.BPM
                        (if p.Mixed then " mixed" else "")
                        p.Pattern
                        p.Density50
                        (p.Density75 * 300.0f / float32 p.BPM)
                        (format_specifics p.Specifics)

                printfn ""

                printfn "This is classed as: %A" (library.Patterns.[next.Chart.Hash].Category)
                match Cache.load next.Chart library.Cache with
                | Some chart ->
                    let d_100 = DifficultyRating.calculate 1.0f chart.Notes |> _.Physical
                    let d_120 = DifficultyRating.calculate 1.2f chart.Notes |> _.Physical
                    let d_80 = DifficultyRating.calculate 0.8f chart.Notes |> _.Physical
                    printfn "%.2f -- %.2f -- %.2f" d_80 d_100 d_120
                    printfn "Characteristic %.2f | %.2f" (d_80 - 0.8 * d_100) (d_120 - 1.2 * d_100)
                | None -> ()


        