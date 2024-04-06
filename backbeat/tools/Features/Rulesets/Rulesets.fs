namespace Backbeat.Features

open System.IO
open System.Collections.Generic
open Prelude
open Prelude.Gameplay
open Backbeat.Utils

module Rulesets =

    let defaultRulesets =
        List.map
            (fun od -> (sprintf "osu-od-%.0f" od, PrefabRulesets.Osu.create od))
            [ 0.0f; 1.0f; 2.0f; 3.0f; 4.0f; 5.0f; 6.0f; 7.0f; 8.0f; 9.0f; 10.0f ]
        @ List.map (fun j -> (sprintf "sc-j%i" j, PrefabRulesets.SC.create j)) [ 2; 3; 4; 5; 6; 7; 8; 9 ]
        @ List.map (fun j -> (sprintf "wife-j%i" j, PrefabRulesets.Wife.create j)) [ 2; 3; 4; 5; 6; 7; 8; 9 ]
        @ List.map
            (fun (d: PrefabRulesets.Ex_Score.Type) ->
                (sprintf "xs-%s" (d.Name.ToLower()), PrefabRulesets.Ex_Score.create d)
            )
            [ PrefabRulesets.Ex_Score.mizu ]
        //@ [ "noodles", PrefabRulesets.Noodles.RULESET ]

    let main_rulesets =
        [ "osu-od-5"; "osu-od-8"; "sc-j4"; "sc-j5"; "wife-j4"; "xs-sdvx" ]

    let compile_rulesets () =
        let data: PrefabRulesets.Repo =
            {
                Rulesets = Dictionary<string, Ruleset>()
            }

        for (name, rs) in defaultRulesets do
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

    open Percyqaz.Shell

    let register (ctx: ShellContext) =
        ctx.WithCommand("compile_rulesets", "Compile ruleset repository", compile_rulesets)
