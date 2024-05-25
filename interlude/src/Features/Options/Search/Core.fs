namespace Interlude.Features.OptionsMenu.Search

open System
open Percyqaz.Flux.UI
open Prelude
open Interlude.UI
open Interlude.UI.Menu

[<AutoOpen>]
module Search =

    let token_match (tokens: string array) (strings: string array) =
        tokens |> Array.exists (fun t -> strings |> Array.exists (fun s -> s.Contains(t, StringComparison.InvariantCultureIgnoreCase)))

    type SearchResult = Widget * int * int * PageWidth

    type SearchResultContainer(height, node_type) =
        inherit Container(node_type)

        interface IHeight with
            member this.Height = height

    let search (search_func: string array -> SearchResult seq) (query: string) : Widget =
        let tokens = query.Split(' ', StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
        let mutable y = 0
        let results = search_func tokens
        if Seq.isEmpty results then
            EmptyState(Icons.SEARCH, %"options.search.no_results", Position = Position.Margin(PRETTY_MARGIN_X, PRETTY_MARGIN_Y))
            |>> Container
            |+ Callout.frame
                (
                    Callout.Normal
                        .Icon(Icons.SEARCH)
                        .Title(%"options.search.no_results.title")
                        .Body(%"options.search.no_results.body")
                )
                (fun (w, h) -> Position.Margin(PRETTY_MARGIN_X, PRETTY_MARGIN_Y * 2.0f).SliceBottom(h).CenterX(w))
            :> Widget
        else
            let content = 
                NavigationContainer.Column<_>(WrapNavigation = false)
                |+ seq {
                    for widget, height, spacing, width in results do
                        yield widget.Pos(y, height, width)
                        y <- y + spacing
                }
                |>> (fun nt -> SearchResultContainer(float32 y * 0.5f * PRETTYHEIGHT, nt))
            ScrollContainer(content, Position = Position.Margin(PRETTY_MARGIN_X, PRETTY_MARGIN_Y))