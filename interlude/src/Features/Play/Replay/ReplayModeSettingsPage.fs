namespace Interlude.Features.Play.Replay

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Interlude.UI

type private ReplayModeSettingsPage() =
    inherit Page()

    override this.Content() : Widget =
        page_container()
            .With(
                PageSetting(%"replay.fixed_scroll_speed", Checkbox fixed_scroll_speed).Pos(0),
                PageSetting(%"replay.input_overlay", Checkbox show_input_overlay).Pos(3),
                PageSetting(%"replay.hit_overlay", Checkbox show_hit_overlay).Pos(5),
                PageSetting(%"replay.hit_overlay_labels", Checkbox show_hit_overlay_labels)
                    .Conditional(show_hit_overlay.Get)
                    .Pos(7),
                PageSetting(%"replay.playfield_dim", Slider.Percent(playfield_dim))
                    .Conditional(fun () -> show_input_overlay.Value || show_hit_overlay.Value)
                    .Pos(9)
            )

    override this.Title = sprintf "%s %s" Icons.SETTINGS (%"replay.settings")
