namespace Interlude.Features.Skins.EditHUD

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Interlude.Content
open Interlude.UI

// currently unused
type BPMPage() =
    inherit Page()

    override this.Content() = Dummy()

    override this.Title = %"hud.bpm"