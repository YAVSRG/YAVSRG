namespace Interlude.Features.Skins.EditHUD

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Interlude.Content
open Interlude.UI

type RateModsPage() =
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

    member this.SaveChanges() =
        Skins.save_hud_config
            { Content.HUD with
                RateModMeterShowMods = show_mods.Value
            }

    override this.Content() =
        this.OnClose(this.SaveChanges)

        page_container()
            .With(
                PageSetting(%"hud.ratemodmeter.showmods", Checkbox show_mods)
                    .Help(Help.Info("hud.ratemodmeter.showmods"))
                    .Pos(0)
            )

    override this.Title = %"hud.ratemodmeter"