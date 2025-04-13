namespace Interlude.Features.OptionsMenu.Search

open System
open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Interlude.UI

[<AutoOpen>]
module Search =

    let token_match (tokens: string array) (strings: string array) : bool =
        tokens |> Array.exists (fun t -> t = "*" || strings |> Array.exists (fun s -> s.Contains(t, StringComparison.InvariantCultureIgnoreCase)))

    type SearchResult = Widget * int * int * PageWidth

    type SearchResultBuilder() =
        member inline _.Zero() : SearchResult seq = Seq.empty
        member inline _.For(en, f) : SearchResult seq = Seq.collect f en
        member inline _.Yield((a, b, c, d): #Widget * int * int * PageWidth) : SearchResult seq = Seq.singleton (a :> Widget, b, c, d)
        member inline _.Yield(v: Widget) : SearchResult seq = Seq.singleton (v, 2, 2, PageWidth.Normal)
        member inline _.YieldFrom(v) : SearchResult seq = v
        member inline _.Delay(f) : SearchResult seq = Seq.delay f
        member inline _.Combine(a, b) : SearchResult seq = Seq.concat [a; b]
    let results = SearchResultBuilder()

    type SearchResultContainer(height: float32, node_type: NodeType) =
        inherit Container(node_type)

        interface IHeight with
            member this.Height = height

        static member Create(height, child: Widget) =
            SearchResultContainer(height, NodeType.Container(K (Some child))).With(child)

    let search (search_func: string array -> SearchResult seq) (query: string) : Widget =
        let tokens = query.Split(' ', StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
        let mutable y = 0
        let results = search_func tokens
        if Seq.isEmpty results then
            Container(NodeType.None)
                .Position(Position.Shrink(PAGE_MARGIN_X, PAGE_MARGIN_Y))
                .With(
                    EmptyState(Icons.SEARCH, %"options.search.no_results"),
                    CalloutCard(
                        Callout.Normal
                            .Icon(Icons.SEARCH)
                            .Title(%"options.search.no_results.title")
                            .Body(%"options.search.no_results.body")
                    )
                        .Position(fun (w, h) -> Position.ShrinkY(PAGE_MARGIN_Y).SliceB(h).SliceX(w))
                )
        else
            NavigationContainer.Column()
                .WrapNavigation(false)
                .With(seq {
                    for widget, height, spacing, width in results do
                        yield widget.Pos(y, height, width)
                        y <- y + spacing
                })
            |> fun results -> SearchResultContainer.Create(float32 y * 0.5f * PAGE_ITEM_HEIGHT, results)
            |> ScrollContainer
            |> _.Position(Position.Shrink(PAGE_MARGIN_X, PAGE_MARGIN_Y))