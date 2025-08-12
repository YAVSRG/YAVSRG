namespace Interlude.Features.Skins

open Interlude.Content
open Interlude.UI
open Interlude.Features.Gameplay

module SkinHotReload =

    let mutable internal skin_select: unit -> Page = Unchecked.defaultof<_>
    let mutable internal noteskin_edit: unit -> Page = Unchecked.defaultof<_>
    //let mutable internal hud_edit: unit -> Page = Unchecked.defaultof<_>

    let hot_reload_noteskin (page: (unit -> #Page) option) =
        Menu.Exit()
        Skins.reload_current_noteskin()
        SelectedChart.recolor()
        skin_select().Show()
        noteskin_edit().Show()
        match page with Some page -> page().Show() | None -> ()

    let init (skin_select_page: unit -> #Page) (noteskin_edit_page: unit -> #Page) =
        skin_select <- fun () -> skin_select_page() :> Page
        noteskin_edit <- fun () -> noteskin_edit_page() :> Page