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

        let stream : PatternSkillBreakdown = PatternSkillBreakdown.Default
        let chordstream : PatternSkillBreakdown = PatternSkillBreakdown.Default
        let jack : PatternSkillBreakdown = PatternSkillBreakdown.Default

        let observe pattern_type (density, accuracy, time, timestamp) =
            match pattern_type with
            | Patterns.CorePatternType.Jack -> jack
            | Patterns.CorePatternType.Chordstream -> chordstream
            | Patterns.CorePatternType.Stream -> stream
            |> PatternSkillBreakdown.observe pattern_type (density, accuracy, time, timestamp)

        for cc_key in library.Cache.Entries.Keys do
            let cc = library.Cache.Entries.[cc_key]
            let data = ScoreDatabase.get cc.Hash score_db
            match data.PersonalBests.TryFind(sc_j4_id) with
            | Some pbs ->
                for (acc, rate, timestamp) in pbs.Accuracy do
                    match library.Cache.Patterns.TryGetValue cc.Hash with
                    | true, res ->
                        for p in res.Patterns do
                            let time = 
                                res.Patterns 
                                |> Seq.filter (fun p2 -> p2.Pattern = p.Pattern && p2.BPM >= p.BPM && p2.Density50 > p.Density50)
                                |> Seq.sumBy _.Amount

                            for octave, time_mult in [| 0.9f, 1.2f; 1.0f, 1.0f; 1.1f, 0.5f |] do
                                observe p.Pattern (p.Density50 * rate * octave, acc, Time.of_number (time / rate * time_mult), timestamp)
                    | false, _ -> ()
            | None -> ()
