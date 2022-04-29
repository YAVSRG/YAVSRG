// load all noteskins in folders

open System.IO
open Prelude.Data.Themes
open Prelude.Common
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Processing

let check_noteskin_textures(ns: Noteskin) =
    for tex in Storage.noteskinTextures do
        match ns.GetTexture tex with
        | Some (img, config) -> ()
        | None -> printfn "Missing texture: %s" tex

let generate_preview(ns: Noteskin, target_file: string) =
    let img : Bitmap = new Bitmap(640, 480)

    let gen_tex (u, v) (bmp : Bitmap, config : TextureConfig) =
        let new_img = new Bitmap(bmp.Width / config.Columns, bmp.Height / config.Rows)
        let u = u % config.Columns
        let v = v % config.Rows
        new_img.Mutate
            ( fun i ->
                i.DrawImage(bmp, Point(new_img.Width * -u, new_img.Height * -v), 1.0f) |> ignore
            )
        new_img.Mutate
            ( fun i ->
                i.Resize(Size(120, 120)) |> ignore
            )
        new_img

    let stretch_tex (height: int) (bmp: Bitmap) =
        bmp.Mutate
            ( fun i ->
                i.Resize(Size(120, height)) |> ignore
            )
        bmp

    let rotate_tex (deg: float32) (bmp: Bitmap) =
        let new_img = bmp.Clone()
        new_img.Mutate
            ( fun i ->
                i.Rotate deg |> ignore
            )
        new_img

    let flip_tex (bmp: Bitmap) =
        let new_img = bmp.Clone()
        new_img.Mutate
            ( fun i ->
                i.Flip FlipMode.Vertical |> ignore
            )
        new_img

    let draw_tex (x, y) (tex: Bitmap) =
        img.Mutate
            ( fun i ->
                i.DrawImage(tex, Point(x, y), 1.0f) |> ignore
            )

    // 4k only for now
    let rotations = 
        if ns.Config.UseRotation then ns.Config.Rotations.[1]
        else Array.zeroCreate 4
        |> Array.map float32

    // todo: playfield

    // receptors
    let receptor = ns.GetTexture("receptor").Value

    use unpressed_receptor = gen_tex(0, 0) receptor
    use r = rotate_tex rotations.[0] unpressed_receptor in draw_tex (80, 360) r
    use r = rotate_tex rotations.[1] unpressed_receptor in draw_tex (200, 360) r
    use r = rotate_tex rotations.[2] unpressed_receptor in draw_tex (320, 360) r
    use pressed_receptor = gen_tex(0, 1) receptor
    use r = rotate_tex rotations.[3] pressed_receptor in draw_tex (440, 360) r

    // notes

    let note = ns.GetTexture("note").Value

    use note_tex = gen_tex(2, 0) note
    use r = rotate_tex rotations.[0] note_tex
    draw_tex (80, 300) r
    use note_tex = gen_tex(2, 2) note
    use r = rotate_tex rotations.[2] note_tex
    draw_tex (320, 240) r
    use note_tex = gen_tex(2, 3) note
    use r = rotate_tex rotations.[3] note_tex
    draw_tex (440, 120) r
    use note_tex = gen_tex(2, 4) note
    use r = rotate_tex rotations.[0] note_tex
    draw_tex (80, 0) r
    use note_tex = gen_tex(2, 5) note
    use r = rotate_tex rotations.[1] note_tex
    draw_tex (200, 0) r

    // hold

    let holdhead = ns.GetTexture("holdhead").Value
    let holdbody = ns.GetTexture("holdbody").Value
    let holdtail = ns.GetTexture("holdtail").Value

    use body = gen_tex(2, 1) holdbody |> stretch_tex 240 in draw_tex (200, 180) body
    use head = gen_tex(2, 1) holdhead
    use r = rotate_tex rotations.[1] head in draw_tex (200, 360) r
    if ns.Config.UseHoldTailTexture then
        use tail = gen_tex(2, 1) holdtail
        use tail = if ns.Config.FlipHoldTail then flip_tex tail else tail
        use r = rotate_tex rotations.[1] tail in draw_tex (200, 120) r
    else
        use head = if ns.Config.FlipHoldTail then flip_tex head else head
        use r = rotate_tex rotations.[1] head in draw_tex (200, 120) r

    // todo: explosions

    img.SaveAsPng target_file

let root = "../../../../"

let skins = ResizeArray<Noteskin.RepoEntry>()

for noteskin_file in Directory.EnumerateFiles(Path.Combine(root, "Noteskins")) do
    let filename = Path.GetFileName noteskin_file
    printfn "Loading: %s" filename
    try
        let ns = Noteskin.FromZipFile noteskin_file
        printfn "Loaded: %s :: Version %s by %s" ns.Config.Name ns.Config.Version ns.Config.Author
        check_noteskin_textures ns
        printfn "Generating preview ..."
        generate_preview (ns, Path.Combine(root, "Previews", (Path.ChangeExtension(filename, ".png"))))

        skins.Add
            {
                Name = ns.Config.Name
                Download = sprintf "https://github.com/YAVSRG/Interlude.Noteskins/raw/main/Noteskins/%s" filename
                Preview = sprintf "https://github.com/YAVSRG/Interlude.Noteskins/raw/main/Previews/%s" (Path.ChangeExtension(filename, ".png"))
            }
        printfn "Added to list!"
    with err -> printfn "Error: %O" err

    printfn "==========================="

printfn "Generating index.json ..."
printfn "Generating index.md ..."