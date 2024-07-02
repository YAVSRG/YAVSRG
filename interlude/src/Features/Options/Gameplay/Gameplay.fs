namespace Interlude.Features.OptionsMenu.Gameplay

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Interlude.Options
open Interlude.UI.Menu
open Interlude.Features.Gameplay
open Interlude.Features.Pacemaker
open Interlude.Features.Skins
open Interlude.Features.Rulesets

type GameplayPage() =
    inherit Page()

    let preview = SkinPreview(SkinPreview.RIGHT_HAND_SIDE 0.35f)

    override this.Content() =
        page_container()
        |+ PageSetting(%"gameplay.scrollspeed", Slider.Percent(options.ScrollSpeed))
            .Tooltip(Tooltip.Info("gameplay.scrollspeed"))
            .Pos(0)
        |+ Text(
            (fun () ->
                [
                    ((1080.0f - options.HitPosition.Value) / options.ScrollSpeed.Value).ToString("F0")
                    (options.ScrollSpeed.Value * 31.0f / 2.38f).ToString("F1")
                    (options.ScrollSpeed.Value * 33.9f / 2.38f).ToString("F1")
                    "C" + (60000.0f * options.ScrollSpeed.Value / Interlude.Content.Content.NoteskinConfig.ColumnWidth).ToString("F0")
                ]
                %> "gameplay.scrollspeed.info"
            ),
            Align = Alignment.CENTER,
            Position = pretty_pos(2, 1, PageWidth.Normal).TrimLeft(PRETTYTEXTWIDTH)
        )
        |+ PageSetting(%"gameplay.hitposition", Slider(options.HitPosition, Step = 1f))
            .Tooltip(Tooltip.Info("gameplay.hitposition"))
            .Pos(3)
        |+ PageSetting(%"gameplay.upscroll", Checkbox options.Upscroll)
            .Tooltip(Tooltip.Info("gameplay.upscroll"))
            .Pos(5)
        |+ PageSetting(%"gameplay.backgrounddim", Slider.Percent(options.BackgroundDim))
            .Tooltip(Tooltip.Info("gameplay.backgrounddim"))
            .Pos(7)
        |+ PageSetting(%"gameplay.hold_to_give_up", Checkbox options.HoldToGiveUp)
            .Tooltip(Tooltip.Info("gameplay.hold_to_give_up"))
            .Pos(9)
        |+ PageSetting(%"gameplay.hide_hit_notes", Checkbox options.VanishingNotes)
            .Tooltip(Tooltip.Info("gameplay.hide_hit_notes"))
            .Pos(11)
        |+ PageButton(%"gameplay.lanecover", (fun () -> LanecoverPage().Show()))
            .Tooltip(Tooltip.Info("gameplay.lanecover"))
            .Pos(14)
        |+ PageButton(%"gameplay.pacemaker", (fun () -> PacemakerOptionsPage().Show()))
            .Tooltip(Tooltip.Info("gameplay.pacemaker").Body(%"gameplay.pacemaker.hint"))
            .Pos(16)
        |+ PageButton(%"rulesets", (fun () -> SelectRulesetPage().Show()))
            .Tooltip(Tooltip.Info("rulesets"))
            .Pos(18)
        |+ PageSetting(%"gameplay.keybinds", GameplayKeybinder.KeymodeAndKeybinder())
            .Tooltip(Tooltip.Info("gameplay.keybinds"))
            .Pos(21, 2, PageWidth.Full)
        |>> Container
        |+ preview
        :> Widget

    override this.Title = %"gameplay"
    override this.OnDestroy() = preview.Destroy()
    override this.OnClose() = ()