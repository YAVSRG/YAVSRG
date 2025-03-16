namespace Prelude.Skins.Repo

open Percyqaz.Data
open Prelude
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Processing
open Prelude.Skins
open Prelude.Skins.Noteskins

module NoteskinPreview =

    let private WIDTH = 640
    let private HEIGHT = 480

    let private NOTE_SIZE = 120
    let private COLUMN_START = (WIDTH - NOTE_SIZE * 4) / 2
    let private HITPOSITION = HEIGHT - NOTE_SIZE

    let private get_texture (id: string) (ns: Noteskin) : Bitmap * int * int =
        match ns.GetTexture id with
        | TextureOk(a, columns, rows) -> a, columns, rows
        | _ -> failwithf "Failed to get texture '%s'" id

    let render (ns: Noteskin) : Bitmap =

        let img: Bitmap = new Bitmap(WIDTH, HEIGHT)

        let gen_tex (u, v) (bmp: Bitmap, columns, rows) =
            let new_img = new Bitmap(bmp.Width / columns, bmp.Height / rows)
            let u = u % columns
            let v = v % rows
            new_img.Mutate(fun i -> i.DrawImage(bmp, Point(new_img.Width * -u, new_img.Height * -v), 1.0f) |> ignore)
            new_img.Mutate(fun i -> i.Resize(Size(NOTE_SIZE, NOTE_SIZE)) |> ignore)
            new_img

        let stretch_tex (height: int) (bmp: Bitmap) =
            bmp.Mutate(fun i -> i.Resize(Size(NOTE_SIZE, height)) |> ignore)
            bmp

        let rotate_tex (deg: float32) (bmp: Bitmap) =
            let new_img = bmp.Clone()
            new_img.Mutate(fun i -> i.Rotate deg |> ignore)
            new_img

        let flip_tex (bmp: Bitmap) =
            let new_img = bmp.Clone()
            new_img.Mutate(fun i -> i.Flip FlipMode.Vertical |> ignore)
            new_img

        let draw_tex (x, y) (tex: Bitmap) =
            img.Mutate(fun i -> i.DrawImage(tex, Point(x, y), 1.0f) |> ignore)

        // 4k only for now
        let rotations =
            if ns.Config.UseRotation then
                ns.Config.Rotations.[1]
            else
                Array.zeroCreate 4
            |> Array.map float32

        // todo: playfield
        // todo: stage left and right if enabled

        // receptors
        let receptor = get_texture "receptor" ns

        use unpressed_receptor = gen_tex (0, 0) receptor
        use r = rotate_tex rotations.[0] unpressed_receptor in
        draw_tex (COLUMN_START, HITPOSITION) r
        use r = rotate_tex rotations.[1] unpressed_receptor in
        draw_tex (COLUMN_START + NOTE_SIZE, HITPOSITION) r
        use r = rotate_tex rotations.[2] unpressed_receptor in
        draw_tex (COLUMN_START + NOTE_SIZE * 2, HITPOSITION) r
        use pressed_receptor = gen_tex (0, 1) receptor
        use r = rotate_tex rotations.[3] pressed_receptor in
        draw_tex (COLUMN_START + NOTE_SIZE * 3, HITPOSITION) r

        // notes

        let note = get_texture "note" ns

        use note_tex = gen_tex (2, 0) note
        use r = rotate_tex rotations.[0] note_tex
        draw_tex (COLUMN_START, 300) r
        use note_tex = gen_tex (2, 2) note
        use r = rotate_tex rotations.[2] note_tex
        draw_tex (COLUMN_START + NOTE_SIZE * 2, 240) r
        use note_tex = gen_tex (2, 3) note
        use r = rotate_tex rotations.[3] note_tex
        draw_tex (COLUMN_START + NOTE_SIZE * 3, 120) r
        use note_tex = gen_tex (2, 4) note
        use r = rotate_tex rotations.[0] note_tex
        draw_tex (COLUMN_START, 0) r
        use note_tex = gen_tex (2, 5) note
        use r = rotate_tex rotations.[1] note_tex
        draw_tex (COLUMN_START + NOTE_SIZE, 0) r

        // hold

        let holdhead = get_texture "holdhead" ns
        let holdbody = get_texture "holdbody" ns

        use body = gen_tex (2, 1) holdbody |> stretch_tex (NOTE_SIZE * 2) in
        draw_tex (COLUMN_START + NOTE_SIZE, 180) body
        use head = gen_tex (2, 1) holdhead
        use r = rotate_tex rotations.[1] head in
        draw_tex (COLUMN_START + NOTE_SIZE, 360) r

        if ns.Config.UseHoldTailTexture then
            let holdtail = get_texture "holdtail" ns
            use tail = gen_tex (2, 1) holdtail
            use tail = if ns.Config.FlipHoldTail then flip_tex tail else tail
            use r = rotate_tex rotations.[1] tail in
            draw_tex (COLUMN_START + NOTE_SIZE, 120) r
        else
            use head = if ns.Config.FlipHoldTail then flip_tex head else head
            use r = rotate_tex rotations.[1] head in
            draw_tex (COLUMN_START + NOTE_SIZE, 120) r

        // todo: explosions

        img

    let render_thumbnail (ns: Noteskin) : Bitmap =
        let bmp, columns, rows = get_texture "note" ns
        let size = min (bmp.Width / columns) NOTE_SIZE
        let thumbnail = new Bitmap(bmp.Width / columns, bmp.Height / rows)
        thumbnail.Mutate(fun i -> i.DrawImage(bmp, Point(0, 0), 1.0f) |> ignore)
        thumbnail.Mutate(fun i -> i.Resize(Size(size, size)) |> ignore)
        thumbnail

[<Json.AutoCodec>]
type SkinVersion =
    {
        Version: string
        Author: string
        Editor: string option
        Preview: string
        Download: string
    }

[<Json.AutoCodec>]
type SkinGroup =
    {
        Name: string
        Versions: SkinVersion list
        Thumbnail: string
    }

[<Json.AutoCodec>]
type SkinRepo =
    {
        Skins: SkinGroup list
    }
    static member Empty = { Skins = [] }

module SkinRepo =

    let add
        (skin_meta: SkinMetadata)
        (name: string)
        (version: string)
        (download_link: string)
        (preview_image_link: string)
        (thumbnail_image_link: string)
        (repo: SkinRepo)
        : SkinRepo * bool =

        let author = skin_meta.Author.Trim()
        let editor = skin_meta.Editor |> Option.map (_.Trim())

        match repo.Skins |> List.tryFind (fun x -> x.Name = name) with
        | Some existing_group ->
            let new_group, new_skin_was_added =
                match
                    existing_group.Versions
                    |> List.tryFind (fun x -> x.Version.ToLower() = version.ToLower())
                with
                | Some existing ->
                    { existing_group with
                        Versions =
                            {
                                Version = version
                                Author = author
                                Editor = editor
                                Preview = preview_image_link
                                Download = download_link
                            }
                            :: (existing_group.Versions |> List.except [ existing ])

                    },
                    false
                | None ->
                    { existing_group with
                        Versions =
                            {
                                Version = version
                                Author = author
                                Editor = editor
                                Preview = preview_image_link
                                Download = download_link
                            }
                            :: existing_group.Versions
                    },
                    true

            { repo with
                Skins = new_group :: (repo.Skins |> List.except [ existing_group ])
            },
            new_skin_was_added
        | None ->
            { repo with
                Skins =
                    {
                        Name = name
                        Versions =
                            [
                                {
                                    Version = version
                                    Author = author
                                    Editor = editor
                                    Preview = preview_image_link
                                    Download = download_link
                                }
                            ]
                        Thumbnail = thumbnail_image_link
                    }
                    :: repo.Skins
            },
            true