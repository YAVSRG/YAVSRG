namespace Interlude.Features.OptionsMenu

open System
open Percyqaz.Flux.UI
open Interlude.UI.Menu

[<AutoOpen>]
module Search =

    let token_match (tokens: string array) (strings: string array) =
        tokens |> Array.exists (fun t -> strings |> Array.exists (fun s -> s.Contains(t, StringComparison.InvariantCultureIgnoreCase)))

    type SearchResult = Widget * int * int * PageWidth

    let search (search_func: string array -> SearchResult seq) (query: string) =
        let tokens = query.Split(' ', StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
        let mutable y = 0
        page_container()
        |+ seq {
            for widget, height, spacing, width in search_func tokens do
                yield widget.Pos(y, height, width)
                y <- y + spacing
        }