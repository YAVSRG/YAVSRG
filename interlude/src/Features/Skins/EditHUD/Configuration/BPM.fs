namespace Interlude.Features.Skins.EditHUD

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Interlude.Content
open Interlude.UI

// currently unused
type BPMPage(on_close: unit -> unit) =
    inherit Page()

    let noteskin_options = Content.HUD

    let pos = Setting.simple noteskin_options.BPMMeterPosition

    override this.Content() = Dummy()

    override this.Title = %"hud.bpm"

    override this.OnClose() =
        on_close ()