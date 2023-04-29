namespace Interlude.Tools.Features

open Percyqaz.Shell
open Prelude.Scoring
open Prelude.Data.Content
open Interlude.Tools

module Assets =

    open System.IO
    open System.IO.Compression

    let make_zip (source: string) (target_zip: string) =
        printfn "Making zip: %s" target_zip
        if File.Exists target_zip then File.Delete target_zip
        ZipFile.CreateFromDirectory(source, target_zip)
        
    let main() =
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

    
    let user_script() =
        let theme = Theme.FromPath(Path.Combine(Utils.ASSETS_PATH, "default"))
        theme.StitchTexture("sc-grade-base", "Rulesets")
        theme.StitchTexture("sc-grade-lamp-overlay", "Rulesets")
        theme.StitchTexture("sc-grade-overlay", "Rulesets")

        let dbar = Noteskin.FromPath(Path.Combine(Utils.ASSETS_PATH, "defaultBar"))
        for t in Storage.noteskinTextures do
            dbar.StitchTexture(t)

    let register(ctx: Context) : Context =
        ctx.WithCommand(
            "asset_script",
            Command.create "debug asset script" [] (Impl.Create user_script)
        ).WithCommand(
            "bundle_assets",
            Command.create "Bundle all assets for build pipeline" [] (Impl.Create main)
        )