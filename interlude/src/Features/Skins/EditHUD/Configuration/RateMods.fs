namespace Interlude.Features.Skins.EditHUD

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Interlude.Content
open Interlude.UI

type RateModsPage(on_close: unit -> unit) =
    inherit Page()

    let config = Content.HUD

    let show_mods = Setting.simple config.RateModMeterShowMods

    let preview =
        { new ElementPreview(config.RateModMeterPosition) with
            override this.DrawComponent(bounds) =
                Text.fill_b (
                    Style.font,
                    (if show_mods.Value then "1.00x, Mirror" else "1.00x"),
                    bounds,
                    Colors.text_subheading,
                    Alignment.CENTER
                )
        }

    override this.Content() =
        page_container()
        |+ PageSetting(%"hud.ratemodmeter.showmods", Checkbox show_mods)
            .Help(Help.Info("hud.ratemodmeter.showmods"))
            .Pos(0)
        :> Widget

    override this.Title = %"hud.ratemodmeter"

    override this.OnClose() =
        Skins.save_hud_config
            { Content.HUD with
                RateModMeterShowMods = show_mods.Value
            }

        on_close ()