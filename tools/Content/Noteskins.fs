namespace YAVSRG.CLI.Features

open System.IO
open Prelude.Content
open Prelude.Content.Noteskins
open Prelude.Content.Noteskins.Repo
open Prelude
open SixLabors.ImageSharp
open YAVSRG.CLI.Utils

module Noteskins =

    let NOTESKINS_ROOT = Path.Combine(YAVSRG_PATH, "backbeat", "noteskins")
    let FIXME_PATH = Path.Combine(NOTESKINS_ROOT, "FixMe")

    let private generate_preview (ns: Noteskin, target_file: string) =
        use img = NoteskinPreview.render ns
        img.SaveAsPng target_file

    let generate_index () =

        let existing_skins =
            match JSON.FromFile(Path.Combine(NOTESKINS_ROOT, "index.json")) with
            | Ok repo -> repo
            | Error e -> NoteskinRepo.Empty

        let mutable updated_repo = existing_skins
        let newly_added = ResizeArray [ "✨ Newly added noteskins ✨" ]

        for noteskin_file in Directory.EnumerateFiles(Path.Combine(NOTESKINS_ROOT, "Noteskins")) |> Seq.sort do
            let filename = Path.GetFileName noteskin_file

            let fixme_folder =
                Path.Combine(FIXME_PATH, Path.GetFileNameWithoutExtension filename)

            if Path.Exists fixme_folder then
                use ns = Noteskin.FromPath fixme_folder
                ns.CompressToZip noteskin_file |> ignore
                Directory.Delete(fixme_folder, true)

            try
                use ns = Noteskin.FromZipStream(File.OpenRead noteskin_file)
                printfn "Loaded: '%s' by %s" ns.Config.Name ns.Config.Author
                let validation = ns.Validate() |> Array.ofSeq

                if validation.Length > 0 then
                    for msg in validation do
                        match msg with
                        | ValidationWarning w -> printfn "Warning: '%s': %s" w.Element w.Message
                        | ValidationError e -> printfn "Error: '%s': %s" e.Element e.Message

                    ns.ExtractToFolder fixme_folder |> ignore
                    let fixme_ns = Noteskin.FromPath fixme_folder

                    fixme_ns.Validate()
                    |> Seq.iter (
                        function
                        | ValidationWarning w -> w.SuggestedFix |> Option.iter (fun s -> s.Action())
                        | ValidationError e -> e.SuggestedFix |> Option.iter (fun s -> s.Action())
                    )

                    fixme_ns.Validate()
                    |> Seq.tryHead
                    |> (function
                    | Some _ -> printfn "Couldn't fix all these issues automatically. Go fix it manually then rerun"
                    | None ->
                        printfn "Fixed all validation issues automatically. Rerun this tool with the fixed version")
                else

                generate_preview (
                    ns,
                    Path.Combine(NOTESKINS_ROOT, "Previews", (Path.ChangeExtension(filename, ".png")))
                )

                let r, new_skin_added =
                    NoteskinRepo.add
                        ns.Config
                        (sprintf "https://github.com/YAVSRG/YAVSRG/raw/main/backbeat/noteskins/Noteskins/%s" filename)
                        (sprintf
                            "https://github.com/YAVSRG/YAVSRG/raw/main/backbeat/noteskins/Previews/%s"
                            (Path.ChangeExtension(filename, ".png")))
                        updated_repo

                updated_repo <- r

                if new_skin_added then
                    newly_added.Add ns.Config.Name
            with err ->
                printfn "Error loading %s: %O" filename err

        if newly_added.Count > 1 then
            File.WriteAllText(Path.Combine(NOTESKINS_ROOT, "new.txt"), String.concat "\n- " newly_added)
        else
            File.WriteAllText(Path.Combine(NOTESKINS_ROOT, "new.txt"), "")

        JSON.ToFile (Path.Combine(NOTESKINS_ROOT, "index.json"), true) updated_repo
        printfn "OK :)"
