namespace Interlude.Tools.Features

open Prelude.Scoring
open Prelude.Data.Themes
open Interlude.Tools

module Bundle_Assets =

    open System.IO
    open System.IO.Compression

    let defaultRulesets =
        List.map
            (fun od -> (sprintf "osu-od-%.0f" od, Rulesets.Osu.create od))
            [0.0f; 1.0f; 2.0f; 3.0f; 4.0f; 5.0f; 6.0f; 7.0f; 8.0f; 9.0f; 10.0f]
        @ List.map
            (fun j -> (sprintf "sc-j%i" j, Rulesets.SC.create j))
            [1; 2; 3; 4; 5; 6; 7; 8; 9]
        @ List.map
            (fun j -> (sprintf "wife-j%i" j, Rulesets.Wife.create j))
            [1; 2; 3; 4; 5; 6; 7; 8; 9]
        @ List.map
            (fun (d: Rulesets.Ex_Score.Type) -> (sprintf "xs-%s" (d.Name.ToLower()), Rulesets.Ex_Score.create d))
            [Rulesets.Ex_Score.sdvx]

    let main_rulesets = ["osu-od-5"; "osu-od-8"; "sc-j4"; "sc-j5"; "wife-j4"; "xs-sdvx"]

    let copy_rulesets() =
        let theme = Theme.FromPath(Path.Combine(Utils.ASSETS_PATH, "default"))
        // todo: clear up orphaned rulesets
        for (name, rs) in defaultRulesets do
            if List.contains name main_rulesets then
                theme.WriteJson(rs, "Rulesets", name + ".ruleset")
            else
                theme.WriteJson(rs, "Rulesets", "More", name + ".ruleset")

    let make_zip (source: string) (target_zip: string) =
        printfn "Making zip: %s" target_zip
        if File.Exists target_zip then File.Delete target_zip
        ZipFile.CreateFromDirectory(source, target_zip)

    let main() =
        copy_rulesets()
        make_zip 
        <| Path.Combine(Utils.ASSETS_PATH, "default")
        <| Path.Combine(Utils.BUILD_RESOURCES_PATH, "default.zip")

        make_zip 
        <| Path.Combine(Utils.ASSETS_PATH, "defaultBar")
        <| Path.Combine(Utils.BUILD_RESOURCES_PATH, "defaultBar.isk")
        make_zip 
        <| Path.Combine(Utils.ASSETS_PATH, "defaultArrow")
        <| Path.Combine(Utils.BUILD_RESOURCES_PATH, "defaultArrow.isk")
        make_zip 
        <| Path.Combine(Utils.ASSETS_PATH, "defaultOrb")
        <| Path.Combine(Utils.BUILD_RESOURCES_PATH, "defaultOrb.isk")