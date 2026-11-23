namespace Interlude.Features.OptionsMenu

open Percyqaz.Flux.UI

[<RequireQualifiedAccess>]
type private OptionsTab =
    | System
    | Gameplay
    | Library
    | Noteskins
    | SearchResults of content: Widget

    static member val MostRecent = OptionsTab.Gameplay with get, set
