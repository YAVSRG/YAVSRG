namespace Interlude.Features.Skins.Browser

open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Prelude

[<AbstractClass>]
type Thumbnail() =
    inherit StaticWidget(NodeType.None)

    let mutable loaded_thumbnail: Sprite option = None
    let fade = Animation.Fade 0.0f

    override this.Init(parent) =
        base.Init parent
        this.Load()

    abstract member Load : unit -> unit

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        fade.Update elapsed_ms

    override this.Draw() =

        match loaded_thumbnail with
        | Some p ->
            Render.sprite
                (Sprite.fill this.Bounds p)
                (Colors.white.O4a fade.Alpha) p
        | None -> ()

    member this.FinishLoading(img: Bitmap) =
        loaded_thumbnail <-
            Some
            <| Sprite.upload_one false LinearSampling (SpriteUpload.OfImage("NOTESKIN_PREVIEW", img))
        fade.Target <- 1.0f

    member this.FinishLoading(sprite: Sprite) =
        loaded_thumbnail <- Some sprite
        fade.Target <- 1.0f

    override this.Finalize() =
        match loaded_thumbnail with
        | Some i -> GameThread.defer (fun () -> Sprite.destroy i |> ignore)
        | None -> ()