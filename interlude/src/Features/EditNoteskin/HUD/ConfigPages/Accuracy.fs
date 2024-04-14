namespace Interlude.Features.EditNoteskin

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Content.Noteskins
open Interlude.Content
open Interlude.UI.Menu
open Interlude.Options

type AccuracyPage(on_close: unit -> unit) as this =
    inherit Page()

    let user_options = options.HUD.Value
    let noteskin_options = Content.NoteskinConfig.HUD

    let pos = Setting.simple noteskin_options.AccuracyPosition

    let grade_colors = Setting.simple user_options.AccuracyGradeColors
    let show_name = Setting.simple user_options.AccuracyShowName

    let preview =
        { new ConfigPreview(0.35f, pos) with
            override this.DrawComponent(bounds) =
                Text.fill (Style.font, "96.72%", bounds.TrimBottom(bounds.Height * 0.3f), Color.White, 0.5f)

                if show_name.Value then
                    Text.fill (Style.font, "SC J4", bounds.SliceBottom(bounds.Height * 0.4f), Color.White, 0.5f)
        }

    do
        this.Content(
            page_container()
            |+ PageSetting("hud.accuracy.gradecolors", Selector<_>.FromBool grade_colors)
                .Pos(0)
                .Tooltip(Tooltip.Info("hud.accuracy.gradecolors"))
            |+ PageSetting("hud.accuracy.showname", Selector<_>.FromBool show_name)
                .Pos(2)
                .Tooltip(Tooltip.Info("hud.accuracy.showname"))
            |>> Container
            |+ preview
        )

    override this.Title = %"hud.accuracy.name"
    override this.OnDestroy() = preview.Destroy()

    override this.OnClose() =
        options.HUD.Set
            { options.HUD.Value with
                AccuracyGradeColors = grade_colors.Value
                AccuracyShowName = show_name.Value
            }

        Noteskins.save_hud_config
            { Content.NoteskinConfig.HUD with
                AccuracyPosition = pos.Value
            }

        on_close ()