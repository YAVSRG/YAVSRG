namespace Interlude.Features.Noteskins.Edit

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Skinning.Noteskins
open Interlude.Content
open Interlude.UI.Menu
open Interlude.Options

type RateModMeterPage(on_close: unit -> unit) =
    inherit Page()

    let user_options = options.HUD.Value
    let noteskin_options = Content.NoteskinConfig.HUD

    let pos = Setting.simple noteskin_options.RateModMeterPosition

    let show_mods = Setting.simple user_options.RateModMeterShowMods

    let preview =
        { new ConfigPreview(0.35f, pos) with
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
            .Tooltip(Tooltip.Info("hud.ratemodmeter.showmods"))
            .Pos(0)
        |>> Container
        |+ preview
        :> Widget

    override this.Title = %"hud.ratemodmeter"
    override this.OnDestroy() = preview.Destroy()

    override this.OnClose() =
        options.HUD.Set
            { options.HUD.Value with
                RateModMeterShowMods = show_mods.Value
            }

        on_close ()