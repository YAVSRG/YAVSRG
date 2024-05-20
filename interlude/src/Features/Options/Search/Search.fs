namespace Interlude.Features.OptionsMenu.Search

open Percyqaz.Flux.UI

module SearchResults =

    let get (query: string) : Widget =
        search (fun tokens -> 
            seq {
                yield! Settings.search_system_settings tokens
                yield! Settings.search_gameplay_settings tokens
                yield! Settings.search_advanced_settings tokens
            }
        ) query