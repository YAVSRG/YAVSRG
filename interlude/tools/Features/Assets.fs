namespace Interlude.Tools.Features

open Percyqaz.Shell
open Prelude.Data.Content
open Interlude.Tools

module Assets =

    open System.IO
    open System.IO.Compression

    let make_zip (source: string) (target_zip: string) =
        printfn "Making zip: %s" target_zip
        if File.Exists target_zip then File.Delete target_zip
        ZipFile.CreateFromDirectory(source, target_zip)

    let cleanup_noteskin_json(id) =
        let ns =
            Path.Combine(Utils.ASSETS_PATH, id) |> Noteskin.FromPath
        ns.Config <- ns.Config
        
    let main() =
        make_zip 
        <| Path.Combine(Utils.ASSETS_PATH, "default")
        <| Path.Combine(Utils.BUILD_RESOURCES_PATH, "default.zip")

        cleanup_noteskin_json "defaultBar"
        make_zip 
        <| Path.Combine(Utils.ASSETS_PATH, "defaultBar")
        <| Path.Combine(Utils.BUILD_RESOURCES_PATH, "defaultBar.isk")
        cleanup_noteskin_json "defaultArrow"
        make_zip 
        <| Path.Combine(Utils.ASSETS_PATH, "defaultArrow")
        <| Path.Combine(Utils.BUILD_RESOURCES_PATH, "defaultArrow.isk")
        cleanup_noteskin_json "defaultOrb"
        make_zip 
        <| Path.Combine(Utils.ASSETS_PATH, "defaultOrb")
        <| Path.Combine(Utils.BUILD_RESOURCES_PATH, "defaultOrb.isk")

    let register(ctx: ShellContext) : ShellContext =
        ctx.WithCommand(
            "bundle_assets",
            "Bundle all assets for build pipeline",
            main
        )