namespace Interlude.Features.Skins.EditHUD

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Interlude.Content
open Interlude.UI

type KeysPerSecondPage(on_close: unit -> unit) =
    inherit Page()

    let config = Content.HUD

    let show_average = Setting.simple config.KeysPerSecondMeterShowAverage
    let show_max = Setting.simple config.KeysPerSecondMeterShowMax
    let show_total = Setting.simple config.KeysPerSecondMeterShowTotal

    override this.Content() =
        page_container()
        |+ PageSetting(%"hud.kps_meter.show_average", Checkbox show_average)
            .Pos(0)
        |+ PageSetting(%"hud.kps_meter.show_max", Checkbox show_max)
            .Pos(2)
        |+ PageSetting(%"hud.kps_meter.show_total", Checkbox show_total)
            .Pos(4)
        :> Widget

    override this.Title = %"hud.kps_meter"

    override this.OnClose() =
        Skins.save_hud_config
            { Content.HUD with
                KeysPerSecondMeterShowAverage = show_average.Value
                KeysPerSecondMeterShowMax = show_max.Value
                KeysPerSecondMeterShowTotal = show_total.Value
            }
        on_close ()