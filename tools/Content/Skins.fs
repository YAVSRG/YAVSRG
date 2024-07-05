namespace YAVSRG.CLI.Features

open System
open System.IO
open Prelude.Skins
open Prelude.Skins.Noteskins
open Prelude.Skins.HudLayouts
open Prelude.Skins.Repo
open Prelude
open SixLabors.ImageSharp
open YAVSRG.CLI.Utils
open YAVSRG.CLI.Features.Backbeat

module Skins =

    let SKINS_ROOT = Path.Combine(YAVSRG_PATH, "backbeat", "skins")

    let private generate_preview (ns: Noteskin, target_file: string) =
        use img = NoteskinPreview.render ns
        img.SaveAsPng target_file

    let private generate_thumbnail (ns: Noteskin, target_file: string) =
        use img = NoteskinPreview.render_thumbnail ns
        img.SaveAsPng target_file

    let add_skin (name: string) =

        let existing_skins =
            match JSON.FromFile(Path.Combine(SKINS_ROOT, "skins.json")) with
            | Ok repo -> repo
            | Error _ -> SkinRepo.Empty

        let skin_folder = Path.Combine(INTERLUDE_SKINS_PATH, name)
        match Skin.FromPath skin_folder with
        | Error err -> raise err
        | Ok skin ->

        let noteskin_folder = Path.Combine(skin_folder, "Noteskin")
        match Noteskin.FromPath noteskin_folder with
        | Error err -> raise err
        | Ok noteskin ->
            
        let validation = noteskin.Validate() |> Array.ofSeq

        if validation.Length > 0 then
            for msg in validation do
                match msg with
                | ValidationWarning w -> 
                    printfn "Warning: '%s': %s" w.Element w.Message
                    match w.SuggestedFix with 
                    | Some fix -> printfn "Applying fix '%s'" fix.Description; fix.Action()
                    | None -> ()
                | ValidationError e -> 
                    printfn "Error: '%s': %s" e.Element e.Message
                    match e.SuggestedFix with 
                    | Some fix -> printfn "Applying fix '%s'" fix.Description; fix.Action()
                    | None -> ()
            failwith "Fix validation errors before uploading"

        let hud_folder = Path.Combine(skin_folder, "HUD")
        if Directory.Exists hud_folder then
            match HudLayout.FromPath hud_folder with
            | Error err -> raise err
            | Ok hud ->
            
                let validation = hud.Validate() |> Array.ofSeq

                if validation.Length > 0 then
                    for msg in validation do
                        match msg with
                        | ValidationWarning w -> 
                            printfn "Warning: '%s': %s" w.Element w.Message
                            match w.SuggestedFix with 
                            | Some fix -> printfn "Applying fix '%s'" fix.Description; fix.Action()
                            | None -> ()
                        | ValidationError e ->
                            printfn "Error: '%s': %s" e.Element e.Message
                            match e.SuggestedFix with 
                            | Some fix -> printfn "Applying fix '%s'" fix.Description; fix.Action()
                            | None -> ()
                    failwith "Fix validation errors before uploading"

        printf "Enter group name for this skin > "
        let group_name = Console.ReadLine()

        printf "Enter version name for this skin (for skins sharing a name) > "
        let version = Console.ReadLine()

        let filename = group_name + "_" + version

        skin.CompressToZip(Path.Combine(SKINS_ROOT, "files", filename + ".isk")) |> ignore

        generate_preview (
            noteskin,
            Path.Combine(SKINS_ROOT, "files", filename + ".png")
        )

        generate_thumbnail (
            noteskin,
            Path.Combine(SKINS_ROOT, "files", (group_name + "__thumbnail.png"))
        )

        let updated_repo, new_skin_added =
            SkinRepo.add
                skin.Metadata
                group_name
                version
                (sprintf "https://github.com/YAVSRG/YAVSRG/raw/main/backbeat/skins/files/%s.isk" filename)
                (sprintf
                    "https://github.com/YAVSRG/YAVSRG/raw/main/backbeat/skins/files/%s.png"
                    filename)
                (sprintf
                    "https://github.com/YAVSRG/YAVSRG/raw/main/backbeat/skins/files/%s"
                    (group_name + "__thumbnail.png"))
                existing_skins

        JSON.ToFile (Path.Combine(SKINS_ROOT, "skins.json"), true) updated_repo
        printfn "OK :)"