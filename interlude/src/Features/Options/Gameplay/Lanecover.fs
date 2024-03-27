namespace Interlude.Features.OptionsMenu.Gameplay

open Percyqaz.Flux.UI
open Interlude.Options
open Interlude.UI.Menu
open Interlude.Utils

type LanecoverPage() as this =
    inherit Page()

    let preview = NoteskinPreview(0.35f, true)

    do
        page_container()
        |+ PageSetting("gameplay.lanecover.enabled", Selector<_>.FromBool options.LaneCover.Enabled)
            .Pos(0)
        |+ PageSetting("gameplay.lanecover.hidden", Slider.Percent(options.LaneCover.Hidden))
            .Tooltip(Tooltip.Info("gameplay.lanecover.hidden"))
            .Pos(3)
        |+ PageSetting("gameplay.lanecover.sudden", Slider.Percent(options.LaneCover.Sudden))
            .Tooltip(Tooltip.Info("gameplay.lanecover.sudden"))
            .Pos(5)
        |+ PageSetting("gameplay.lanecover.fadelength", Slider(options.LaneCover.FadeLength, Step = 5.0f))
            .Tooltip(Tooltip.Info("gameplay.lanecover.fadelength"))
            .Pos(7)
        |+ PageSetting("gameplay.lanecover.color", ColorPicker(options.LaneCover.Color, true))
            .Pos(9, 3)
        |>> Container
        |+ preview
        |> this.Content

    override this.Title = %"gameplay.lanecover.name"
    override this.OnDestroy() = preview.Destroy()
    override this.OnClose() = ()