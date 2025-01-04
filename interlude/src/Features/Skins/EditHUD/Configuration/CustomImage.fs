namespace Interlude.Features.Skins.EditHUD

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Interlude.Content
open Interlude.UI

type CustomImagePage(on_close: unit -> unit) =
    inherit Page()

    let config = Content.HUD
    let frame_time = 
        config.CustomImageFrameTime
        |> Setting.bounded (10.0f<ms / rate>, 2000.0f<ms / rate>)
    
    override this.Content() =
        page_container()
        |+ PageSetting(%"hud.custom_image.frame_time", Slider(frame_time |> Setting.uom, Step = 1f))
            .Pos(0)
        
        :> Widget

    override this.Title = %"hud.custom_image"

    override this.OnClose() =
        Skins.save_hud_config 
            { Content.HUD with
                CustomImageFrameTime = frame_time.Value
            }
        on_close ()
