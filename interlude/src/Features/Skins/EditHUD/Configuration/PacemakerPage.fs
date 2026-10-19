namespace Interlude.Features.Skins.EditHUD

open Percyqaz.Flux.UI
open Prelude
open Interlude.UI

// currently unused
type PacemakerPage() =
    inherit Page()

    override this.Content() : Widget = Dummy()

    override this.Title = %"hud.pacemaker"
