namespace Interlude.Features.OptionsMenu.Gameplay

open Percyqaz.Flux.UI
open Prelude
open Interlude.Options
open Interlude.UI.Menu
open Interlude.Features.EditNoteskin

type LanecoverPage() =
    inherit Page()

    let preview = NoteskinPreview(NoteskinPreview.RIGHT_HAND_SIDE 0.35f)

    override this.Content() =
        page_container()
        |+ PageSetting("gameplay.lanecover.enabled", Checkbox options.LaneCover.Enabled)
            .Pos(0)
        |+ PageSetting("gameplay.lanecover.draw_under_receptors", Checkbox options.LaneCover.DrawUnderReceptors)
            .Pos(2)
        |+ PageSetting("gameplay.lanecover.hidden", Slider.Percent(options.LaneCover.Hidden))
            .Tooltip(Tooltip.Info("gameplay.lanecover.hidden"))
            .Pos(5)
        |+ PageSetting("gameplay.lanecover.sudden", Slider.Percent(options.LaneCover.Sudden))
            .Tooltip(Tooltip.Info("gameplay.lanecover.sudden"))
            .Pos(7)
        |+ PageSetting("gameplay.lanecover.fadelength", Slider(options.LaneCover.FadeLength, Step = 5.0f))
            .Tooltip(Tooltip.Info("gameplay.lanecover.fadelength"))
            .Pos(9)
        |+ PageSetting("gameplay.lanecover.color", ColorPicker(options.LaneCover.Color, true))
            .Pos(11, 3)
        |>> Container
        |+ preview
        :> Widget

    override this.Title = %"gameplay.lanecover"
    override this.OnDestroy() = preview.Destroy()
    override this.OnClose() = ()