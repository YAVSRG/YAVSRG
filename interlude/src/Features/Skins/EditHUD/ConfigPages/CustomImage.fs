namespace Interlude.Features.Skins.EditHUD

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Interlude.Content
open Interlude.UI

type CustomImagePage(on_close: unit -> unit) =
    inherit Page()

    let config = Content.HUD
    let frame_time = Setting.simple config.CustomImageFrameTime |> Setting.bound 10.0f 2000.0f
    
    override this.Content() =
        page_container()
        |+ PageSetting(%"hud.customimage.frame_time", Slider(frame_time, Step = 1f))
            .Pos(0)
        
        :> Widget

    override this.Title = %"hud.customimage"

    override this.OnClose() =
        Skins.save_hud_config 
            { Content.HUD with
                CustomImageFrameTime = frame_time.Value
            }
        on_close ()
