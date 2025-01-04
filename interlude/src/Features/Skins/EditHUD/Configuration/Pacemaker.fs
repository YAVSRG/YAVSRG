namespace Interlude.Features.Skins.EditHUD

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Interlude.Content
open Interlude.UI

// currently unused
type PacemakerPage(on_close: unit -> unit) =
    inherit Page()

    let config = Content.HUD

    let pos = Setting.simple config.PacemakerPosition

    override this.Content() = Dummy()

    override this.Title = %"hud.pacemaker"

    override this.OnClose() =
        on_close ()