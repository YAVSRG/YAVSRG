namespace YAVSRG.CLI.Features

open System.IO
open System.Collections.Generic
open Prelude
open Prelude.Gameplay
open Prelude.Gameplay.PremadeRulesets
open Prelude.Backbeat
open YAVSRG.CLI.Utils

module Rulesets =

    let RULESETS_PATH = Path.Combine(YAVSRG_PATH, "backbeat", "rulesets")

    let DEFAULT_RULESETS =
        List.map
            (fun od -> (sprintf "osu-od-%.0f" od, ``osu!``.create od ``osu!``.NoMod ))
            [ 0.0f; 1.0f; 2.0f; 3.0f; 4.0f; 5.0f; 6.0f; 7.0f; 8.0f; 9.0f; 10.0f ]
        @ List.map (fun j -> (sprintf "sc-j%i" j, SC.create j)) [ 2; 3; 4; 5; 6; 7; 8; 9 ]
        @ List.map (fun j -> (sprintf "wife-j%i" j, Wife3.create j)) [ 2; 3; 4; 5; 6; 7; 8; 9 ]
        @ List.map
            (fun (d: Ex_Score.Type) ->
                (sprintf "xs-%s" (d.Name.ToLower()), Ex_Score.create d)
            )
            [ Ex_Score.mizu ]
        @ [ "noodles", Noodles.RULESET ]

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
