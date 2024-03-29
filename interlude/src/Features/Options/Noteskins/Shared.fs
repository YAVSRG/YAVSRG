namespace Interlude.Features.OptionsMenu.Noteskins

open Percyqaz.Flux.UI
open Prelude
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Content

[<AutoOpen>]
module Shared =
    let mutable choose_noteskins: unit -> unit = ignore

    let private noteskin_required =
        Callout
            .Small
            .Title(%"hud.noteskin_required.title")
            .Body(%"hud.noteskin_required.element")
            .Button(%"hud.noteskin_required.button", fun () -> Menu.Back(); Menu.Back(); choose_noteskins())

    let or_require_noteskin (items: Widget list) : Widget list =
        if Content.Noteskin.IsEmbedded then
            [ Callout.frame noteskin_required (fun (w, h) -> Position.Box(0.0f, 1.0f, 100.0f, -200.0f - h, w, h)) ]
        else items