namespace Interlude.Features.OptionsMenu.Gameplay

open Percyqaz.Flux.UI
open Prelude
open Interlude.Options
open Interlude.UI
open Interlude.Features.Skins

type LanecoverPage() =
    inherit Page()

    override this.Content() =
        let preview = new SkinPreview(SkinPreview.RIGHT_HAND_SIDE 0.35f)
        this.DisposeOnDestroy(preview)
        page_container()
            .With(
                PageSetting(%"gameplay.lanecover.enabled", Checkbox options.LaneCover.Enabled)
                    .Pos(0),
                PageSetting(%"gameplay.lanecover.draw_under_receptors", Checkbox options.LaneCover.DrawUnderReceptors)
                    .Pos(2),
                PageSetting(%"gameplay.lanecover.hidden", Slider.Percent(options.LaneCover.Hidden))
                    .Help(Help.Info("gameplay.lanecover.hidden"))
                    .Pos(5),
                PageSetting(%"gameplay.lanecover.sudden", Slider.Percent(options.LaneCover.Sudden))
                    .Help(Help.Info("gameplay.lanecover.sudden"))
                    .Pos(7),
                PageSetting(%"gameplay.lanecover.fadelength", Slider(options.LaneCover.FadeLength, Step = 5.0f))
                    .Help(Help.Info("gameplay.lanecover.fadelength"))
                    .Pos(9),
                PageSetting(%"gameplay.lanecover.color", ColorPicker(%"gameplay.lanecover.color", options.LaneCover.Color, true))
                    .Pos(11)
            )
        |> Container.Create
        |+ preview
        :> Widget

    override this.Title = %"gameplay.lanecover"