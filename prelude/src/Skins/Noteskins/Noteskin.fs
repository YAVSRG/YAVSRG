namespace Prelude.Skins.Noteskins

open System.IO
open System.IO.Compression
open SixLabors.ImageSharp.Processing
open Percyqaz.Common
open Percyqaz.Data
open Prelude
open Prelude.Skins

type Noteskin(storage: StorageType) as this =
    inherit Storage(storage)

    let mutable config: NoteskinConfig = NoteskinConfig.Default

    do this.ReloadFromDisk()

    member this.Config
        with set (conf: NoteskinConfig) =
            config <- conf
            this.WriteJson(config, "noteskin.json")
        and get (): NoteskinConfig = config

    override this.ReloadFromDisk() : unit =
        base.ReloadFromDisk()

        config <-
            match
                this.TryGetJson<NoteskinExplosionMigration.NoteskinMigrationModel>(false, "noteskin.json"),
                this.TryGetJson<NoteskinConfig>(true, "noteskin.json")
            with
            | Some migration_patch, Some data -> NoteskinExplosionMigration.patch migration_patch data.Validate
            | _ -> failwith "noteskin.json was missing or didn't load properly"

    member this.GetTexture(name: string) : TextureLoadResult =
        this.LoadTexture(name, NoteskinTextureRules.get this.Config name)

    member this.RequiredTextures: string seq =
        NoteskinTextureRules.list() |> Seq.filter(NoteskinTextureRules.get this.Config >> _.IsRequired)

    member this.Validate() : ValidationMessage seq =
        seq {
            for texture_id in NoteskinTextureRules.list() do
                yield! this.ValidateTexture(texture_id, NoteskinTextureRules.get this.Config texture_id)

            yield! this.ValidateHoldTail()
        }

    member private this.ValidateHoldTail() : ValidationMessage seq =
        seq {
            match this.GetTexture("holdtail") with
            | TextureNotRequired
            | TextureError _ -> ()
            | TextureOk(bmp, rows, columns) ->
                let x, y = bmp.Width / columns / 2, bmp.Height / rows / 2

                if
                    this.Config.UseHoldTailTexture
                    && not this.Config.FlipHoldTail
                    && bmp.Height / rows > 24
                    && bmp.[x, y - 8].A > 127uy
                    && bmp.[x, y + 8].A < 127uy
                then
                    yield
                        ValidationWarning
                            {
                                Element = "holdtail"
                                Message =
                                    "Looks like your holdtail is pointing up.\nIt should point down and flip when using downscroll.\nThis will make it look right on both scroll directions."
                                SuggestedFix =
                                    Some
                                        {
                                            Description = "Apply fix"
                                            Action =
                                                fun () ->
                                                    if this.TextureIsGrid("holdtail") then
                                                        this.SplitTexture("holdtail")

                                                        this.MutateLooseTextures(
                                                            (fun (ctx: IImageProcessingContext) ->
                                                                ctx.Flip(FlipMode.Vertical)
                                                            ),
                                                            "holdtail"
                                                        )
                                                        |> ignore

                                                        this.StitchTexture("holdtail")
                                                    else
                                                        this.MutateLooseTextures(
                                                            (fun (ctx: IImageProcessingContext) ->
                                                                ctx.Flip(FlipMode.Vertical)
                                                            ),
                                                            "holdtail"
                                                        )
                                                        |> ignore

                                                    this.Config <- { this.Config with FlipHoldTail = true }
                                        }
                            }

        }

    static member FromZipStream(stream: Stream) : Noteskin =
        new Noteskin(Embedded(new ZipArchive(stream)))

    static member FromPath(path: string) : Result<Noteskin, exn> =
        try
            new Noteskin(Folder path) |> Ok
        with err ->
            Error err

    static member Exists(path: string) : bool =
        Directory.Exists(path) && File.Exists(Path.Combine(path, "noteskin.json"))
