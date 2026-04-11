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