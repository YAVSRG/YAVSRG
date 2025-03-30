namespace Interlude.UI

open System.Runtime.CompilerServices
open Percyqaz.Flux.UI

[<AutoOpen>]
module PageLayout =

    [<Struct>]
    [<RequireQualifiedAccess>]
    type PageWidth =
        | Normal
        | Full
        | Custom of float32

    let PAGE_ITEM_HEIGHT = 65.0f
    let PAGE_LABEL_WIDTH = PAGE_ITEM_HEIGHT * 6.0f
    let PAGE_ITEM_WIDTH = 1080.0f

    let PAGE_BOTTOM = 23

    let PAGE_MARGIN_Y = (1080.0f - (float32 PAGE_BOTTOM * 0.5f * PAGE_ITEM_HEIGHT)) * 0.5f
    let PAGE_MARGIN_X = 100.0f

    let page_position (start: int, height: int, width: PageWidth) : Position =
        match width with
        | PageWidth.Normal ->
            Position.Box(0.0f, 0.0f, 0.0f, float32 start * 0.5f * PAGE_ITEM_HEIGHT, PAGE_ITEM_WIDTH, float32 height * 0.5f * PAGE_ITEM_HEIGHT)
        | PageWidth.Full ->
            Position.SliceT(float32 start * 0.5f * PAGE_ITEM_HEIGHT, float32 height * 0.5f * PAGE_ITEM_HEIGHT)
        | PageWidth.Custom w ->
            Position.Box(0.0f, 0.0f, 0.0f, float32 start * 0.5f * PAGE_ITEM_HEIGHT, w, float32 height * 0.5f * PAGE_ITEM_HEIGHT)

    let page_container () : NavigationContainer.Column =
        NavigationContainer.Column()
            .WrapNavigation(false)
            .Position(Position.Shrink(PAGE_MARGIN_X, PAGE_MARGIN_Y))

    // todo: make this a component instead of this goofy ass
    let refreshable_row (get_count: unit -> int) (constructor: int -> int -> Widget) : NavigationContainer.Row * (unit -> unit) =
        let r = NavigationContainer.Row()

        let refresh () =
            r.Clear()
            let n = get_count ()

            for i in 0 .. (n - 1) do
                r.Add(constructor i n)

        refresh ()
        r, refresh

[<Extension>]
type PageLayoutExtensions =

    [<Extension>]
    static member Pos(widget: #Widget, y: int) : #Widget =
        widget.Position <- page_position (y, 2, PageWidth.Normal)
        widget

    [<Extension>]
    static member Pos(widget: #Widget, y: int, h: int) : #Widget =
        widget.Position <- page_position (y, h, PageWidth.Normal)
        widget

    [<Extension>]
    static member Pos(widget: #Widget, y: int, h: int, width: PageWidth) : #Widget =
        widget.Position <- page_position (y, h, width)
        widget

    [<Extension>]
    static member Pos(card: CalloutCard, y: int) : CalloutCard =
        card.Position <-
            page_position (
                y,
                (card :> IHeight).Height / PAGE_ITEM_HEIGHT * 2.0f |> ceil |> int,
                PageWidth.Custom (card :> IWidth).Width
            )
        card