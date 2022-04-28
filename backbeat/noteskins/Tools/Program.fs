// load all noteskins in folders

open System.IO
open Prelude.Data.Themes

let check_noteskin_textures(ns: Noteskin) =
    for tex in Storage.noteskinTextures do
        match ns.GetTexture tex with
        | Some (img, config) -> ()
        | None -> printfn "Missing texture: %s" tex

let root = "../../../../"

for noteskin_file in Directory.EnumerateFiles(Path.Combine(root, "Noteskins")) do
    let filename = Path.GetFileName noteskin_file
    printfn "Loading: %s" filename
    try
        let ns = Noteskin.FromZipFile noteskin_file
        printfn "Loaded: %s :: Version %s by %s" ns.Config.Name ns.Config.Version ns.Config.Author
        check_noteskin_textures ns
        printfn "Todo: Generating preview ..."
        printfn "Added to list!"
    with err -> printfn "Error: %O" err

    printfn "==========================="

printfn "Generating index.json ..."
printfn "Generating index.md ..."