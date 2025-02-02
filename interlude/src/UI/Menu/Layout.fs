namespace Interlude.UI

open Percyqaz.Flux.UI

[<AutoOpen>]
module PageLayout =

    [<Struct>]
    [<RequireQualifiedAccess>]
    type PageWidth =
        | Normal
        | Full
        | Custom of float32

    let PRETTYHEIGHT = 65.0f
    let PRETTYTEXTWIDTH = PRETTYHEIGHT * 6.0f
    let PRETTYWIDTH = 1080.0f

    let PAGE_BOTTOM = 23

    let PRETTY_MARGIN_Y = (1080.0f - (float32 PAGE_BOTTOM * 0.5f * PRETTYHEIGHT)) * 0.5f
    let PRETTY_MARGIN_X = 100.0f

    let pretty_pos(start: int, height: int, width: PageWidth) =
        match width with
        | PageWidth.Normal ->
            Position.Box(0.0f, 0.0f, 0.0f, float32 start * 0.5f * PRETTYHEIGHT, PRETTYWIDTH, float32 height * 0.5f * PRETTYHEIGHT)
        | PageWidth.Full ->
            Position.SliceT(float32 start * 0.5f * PRETTYHEIGHT, float32 height * 0.5f * PRETTYHEIGHT)
        | PageWidth.Custom w ->
            Position.Box(0.0f, 0.0f, 0.0f, float32 start * 0.5f * PRETTYHEIGHT, w, float32 height * 0.5f * PRETTYHEIGHT)

    type Widget with

        member this.Pos(y: int) =
            this.Position <- pretty_pos (y, 2, PageWidth.Normal)
            this

        member this.Pos(y: int, h: int) =
            this.Position <- pretty_pos (y, h, PageWidth.Normal)
            this

        member this.Pos(y: int, h: int, width: PageWidth) =
            this.Position <- pretty_pos (y, h, width)
            this