namespace YAVSRG.CLI.Features

open System.IO
open System.Collections.Generic
open Prelude
open Prelude.Gameplay.Rulesets
open Prelude.Backbeat
open YAVSRG.CLI.Utils

module Rulesets =

    let RULESETS_PATH = Path.Combine(YAVSRG_PATH, "backbeat", "rulesets")

    let DEFAULT_RULESETS = []

    let generate_index () =
        let data: RulesetRepo =
            {
                Rulesets = Dictionary<string, Ruleset>()
            }

        for (name, rs) in DEFAULT_RULESETS do
            data.Rulesets.Add(name, rs)

        for file in Directory.GetFiles(RULESETS_PATH) do
            if file.EndsWith(".ruleset") then
                let f = Path.GetFileNameWithoutExtension(file)

                let d =
                    match JSON.FromFile<Ruleset> file with
                    | Ok d -> d
                    | Error e -> failwithf "Error loading '%s': %A" f e

                data.Rulesets.Add(f, d)

        JSON.ToFile (Path.Combine(RULESETS_PATH, "rulesets.json"), true) data
        printfn "Done."
