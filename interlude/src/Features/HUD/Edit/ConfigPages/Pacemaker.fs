namespace Interlude.Features.Noteskins.Edit

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Skinning.Noteskins
open Interlude.Content
open Interlude.UI
open Interlude.UI.Menu

// currently unused
type PacemakerPage(on_close: unit -> unit) =
    inherit Page()

    let config = Content.NoteskinConfig.HUD

    let pos = Setting.simple config.PacemakerPosition

    let preview =
        { new ConfigPreview(0.35f, pos) with
            override this.DrawComponent(bounds) =
                Text.fill_b (Style.font, Icons.FLAG, bounds, Colors.text, Alignment.CENTER)
        }

    override this.Content() = preview

    override this.Title = %"hud.pacemaker"
    override this.OnDestroy() = preview.Destroy()

    override this.OnClose() =
        on_close ()