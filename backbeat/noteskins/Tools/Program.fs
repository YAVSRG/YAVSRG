open System.IO
open Percyqaz.Common
open Prelude.Content
open Prelude.Content.Noteskins
open Prelude.Content.Noteskins.Repo
open Prelude.Common
open SixLabors.ImageSharp

let root =
    let dir = Path.GetFileName(Directory.GetCurrentDirectory())

    match dir with
    | "Tools" -> "../"
    | _ -> "../../../../"

Path.Combine(root, "Noteskins") |> Path.GetFullPath |> printfn "%s"

let generate_preview (ns: Noteskin, target_file: string) =
    use img = NoteskinPreview.render ns
    img.SaveAsPng target_file

let newly_added = ResizeArray [ "✨ Newly added noteskins ✨" ]

let existing_skins =
    match JSON.FromFile(Path.Combine(root, "index.json")) with
    | Ok repo -> repo
    | Error e -> NoteskinRepo.Empty

let mutable updated_repo = existing_skins

let fix_me_path = Path.Combine(root, "FixMe")

for noteskin_file in Directory.EnumerateFiles(Path.Combine(root, "Noteskins")) |> Seq.sort do
    let filename = Path.GetFileName noteskin_file

    let fixme_folder =
        Path.Combine(fix_me_path, Path.GetFileNameWithoutExtension filename)

    if Path.Exists fixme_folder then
        use ns = Noteskin.FromPath fixme_folder
        ns.CompressToZip noteskin_file |> ignore
        Directory.Delete(fixme_folder, true)

    try
        use ns = Noteskin.FromZipStream(File.OpenRead noteskin_file)
        Logging.Info(sprintf "Loaded: '%s' by %s" ns.Config.Name ns.Config.Author)
        let validation = ns.Validate() |> Array.ofSeq

        if validation.Length > 0 then
            for msg in validation do
                match msg with
                | ValidationWarning w -> Logging.Warn(sprintf "'%s': %s" w.Element w.Message)
                | ValidationError e -> Logging.Error(sprintf "'%s': %s" e.Element e.Message)

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
            | Some _ -> Logging.Warn("Couldn't fix all these issues automatically. Go fix it manually then rerun")
            | None -> Logging.Info("Fixed all validation issues automatically. Rerun this tool with the fixed version"))
        else

        generate_preview (ns, Path.Combine(root, "Previews", (Path.ChangeExtension(filename, ".png"))))

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
        Logging.Error(sprintf "Error loading %s: %O" filename err)

    Logging.Info ""

if newly_added.Count > 1 then
    File.WriteAllText(Path.Combine(root, "new.txt"), String.concat "\n- " newly_added)
else
    File.WriteAllText(Path.Combine(root, "new.txt"), "")

JSON.ToFile (Path.Combine(root, "index.json"), true) updated_repo
Logging.Info "OK :)"
Logging.Shutdown()
