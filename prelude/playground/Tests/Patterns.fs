namespace Prelude.Test

open System
open System.IO
open Prelude
open Prelude.Charts.Processing
open Percyqaz.Data.Sqlite
open Prelude.Gameplay
open Prelude.Data.Library
open Prelude.Data

module Patterns =

    let run () =

        Directory.SetCurrentDirectory("C:/Interlude/dev") // this is my interlude install location
        let library = Library.load()
        let score_db = ScoreDatabase.create true (Database.from_file "Data/scores.db")

        let sc_j4 = PremadeRulesets.SC.create 4
        let sc_j4_id = Ruleset.hash sc_j4

        let mutable stream = PatternStats.create()
        let mutable chordstream = PatternStats.create()
        let mutable jack = PatternStats.create()

        let observe pattern_type (bpm: float32, density, accuracy, time) =
            if time > 5000.0f<ms> then

                let scaled_density =
                    if pattern_type = Patterns.CorePatternType.Jack then
                        density * 15f / float32 bpm 
                    else density * 30f / float32 bpm

                match pattern_type with 
                | Patterns.CorePatternType.Jack ->
                    jack <- jack |> PatternStats.add_observation (bpm, scaled_density, accuracy, time)
                | Patterns.CorePatternType.Chordstream ->
                    chordstream <- chordstream |> PatternStats.add_observation (bpm, scaled_density, accuracy, time)
                | Patterns.CorePatternType.Stream ->
                    stream <- stream |> PatternStats.add_observation (bpm, scaled_density, accuracy, time)

        for cc_key in library.Cache.Entries.Keys do
            let cc = library.Cache.Entries.[cc_key]
            let data = ScoreDatabase.get cc.Hash score_db
            match data.PersonalBests.TryFind(sc_j4_id) with
            | Some pbs ->
                match PersonalBests.get_best_above_with_rate 1.0f pbs.Accuracy with
                | Some (acc, rate) ->
                    match library.Cache.Patterns.TryGetValue cc.Hash with
                    | true, res ->
                        for p in res.Patterns do
                            let time = res.Patterns |> Seq.filter (fun p2 -> p2.Pattern = p.Pattern && p2.BPM >= p.BPM && p2.Density50 > p.Density50) |> Seq.sumBy (_.Amount)
                            observe p.Pattern (float32 p.BPM * rate, p.Density50, acc, Time.of_number (time / rate))
                    | false, _ -> ()
                | None -> ()
            | None -> ()
            
        for k in stream |> Seq.sortDescending do
            let (bpm, density, control, time) = k
            
            printfn "%iBPM STREAM (%.2f%%, %.2f%%): %.0fs" 
                bpm 
                (density * 100.0f) 
                (control * 100.0) 
                (time / 1000.0f<ms>)

        for k in chordstream |> Seq.sortDescending do
            let (bpm, density, control, time) = k

            printfn "%iBPM CHORDSTREAM (%.2f%%, %.2f%%): %.0fs" 
                bpm 
                (density * 100.0f)
                (control * 100.0)
                (time / 1000.0f<ms>)

        for k in jack |> Seq.sortDescending do
            let (bpm, density, control, time) = k

            printfn "%iBPM JACK (%.2f%%, %.2f%%): %.0fs"
                bpm
                (density * 100.0f)
                (control * 100.0)
                (time / 1000.0f<ms>)
