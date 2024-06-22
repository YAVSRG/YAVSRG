namespace Interlude.Features.Noteskins.Edit

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Skinning.Noteskins
open Interlude.Content
open Interlude.UI.Menu
open Interlude.Options

type InputMeterPage(on_close: unit -> unit) =
    inherit Page()

    let user_options = options.HUD.Value
    let noteskin_options = Content.NoteskinConfig.HUD

    let scroll_speed = Setting.bounded user_options.InputMeterScrollSpeed 0.5f 3.0f
    
    override this.Content() =
        page_container()
        |+ PageSetting(%"hud.inputmeter.scroll_speed", Slider.Percent scroll_speed)
            .Pos(0)
        :> Widget

    override this.Title = %"hud.inputmeter"

    override this.OnClose() =
        options.HUD.Set
            { options.HUD.Value with
                InputMeterScrollSpeed = scroll_speed.Value
            }
        on_close ()
