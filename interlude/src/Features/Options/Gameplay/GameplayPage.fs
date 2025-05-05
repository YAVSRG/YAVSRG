namespace Interlude.Features.OptionsMenu.Gameplay

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Interlude.Options
open Interlude.UI
open Interlude.Features.Pacemaker
open Interlude.Features.Skins
open Interlude.Features.Rulesets

type GameplayPage() =
    inherit Page()

    static member ScrollSpeed() : PageSetting =
        PageSetting(%"gameplay.scrollspeed", Slider.Percent(Setting.uom options.ScrollSpeed))
            .With(
                Text(fun () ->
                    [
                        ((1080.0f - options.HitPosition.Value) / float32 options.ScrollSpeed.Value).ToString("F0")
                        (float32 options.ScrollSpeed.Value * 12.698412f).ToString("F1")
                        (float32 options.ScrollSpeed.Value * 33.9f / 2.38f).ToString("F1")
                        "C" + (60000.0f * float32 options.ScrollSpeed.Value / Interlude.Content.Content.NoteskinConfig.DefaultColumnWidth).ToString("F0")
                    ]
                    %> "gameplay.scrollspeed.info"
                )
                    .Align(Alignment.CENTER)
                    .Position(page_position(2, 1, PageWidth.Normal).ShrinkL(PAGE_LABEL_WIDTH))
            )
            .Help(Help.Info("gameplay.scrollspeed"))

    static member HitPosition() : PageSetting =
        PageSetting(%"gameplay.hitposition", Slider(options.HitPosition, Step = 1f))
            .Help(Help.Info("gameplay.hitposition"))

    static member Upscroll() : PageSetting =
        PageSetting(%"gameplay.upscroll", Checkbox options.Upscroll)
            .Help(Help.Info("gameplay.upscroll"))

    static member BackgroundDim() : PageSetting =
        PageSetting(%"gameplay.backgrounddim", Slider.Percent(options.BackgroundDim))
            .Help(Help.Info("gameplay.backgrounddim"))

    static member HoldToGiveUp() : PageSetting =
        PageSetting(%"gameplay.hold_to_give_up", Checkbox options.HoldToGiveUp)
            .Help(Help.Info("gameplay.hold_to_give_up"))

    static member HideHitNotes() : PageSetting =
        PageSetting(%"gameplay.hide_hit_notes", Checkbox options.VanishingNotes)
            .Help(Help.Info("gameplay.hide_hit_notes"))

    static member OnQuitOut() : PageSetting =
        PageSetting(%"gameplay.on_quit_out",
            SelectDropdown(
                [|
                    QuitOutBehaviour.SaveAndShow, %"gameplay.on_quit_out.save_and_show"
                    QuitOutBehaviour.Show, %"gameplay.on_quit_out.show"
                    QuitOutBehaviour.Ignore, %"gameplay.on_quit_out.ignore"
                |],
                options.QuitOutBehaviour
            )
        )

    static member Lanecover() : PageButton =
        PageButton(%"gameplay.lanecover", fun () -> LanecoverPage().Show())
            .Help(Help.Info("gameplay.lanecover"))

    static member Pacemaker() : PageButton =
        PageButton(%"gameplay.pacemaker", fun () -> PacemakerOptionsPage().Show())
            .Help(Help.Info("gameplay.pacemaker"))

    static member Keybinds() : PageButton =
        PageButton(%"gameplay.keybinds", fun () -> GameplayBindsPage().Show())
            .Help(Help.Info("gameplay.keybinds"))

    override this.Content() =
        let preview = new SkinPreview(SkinPreview.RIGHT_HAND_SIDE(0.35f).TranslateY(200.0f))
        this.DisposeOnDestroy(preview)
        page_container()
            .With(
                GameplayPage.ScrollSpeed().Pos(0),
                GameplayPage.HitPosition().Pos(3),
                GameplayPage.Upscroll().Pos(5),
                GameplayPage.BackgroundDim().Pos(7),
                GameplayPage.HoldToGiveUp().Pos(9),
                GameplayPage.HideHitNotes().Pos(11),
                GameplayPage.OnQuitOut().Pos(13),
                GameplayPage.Lanecover().Pos(16),
                GameplayPage.Pacemaker().Pos(18),
                GameplayPage.Keybinds().Pos(21)
            )
        |> Container.Create
        |+ preview
        :> Widget

    override this.Title = %"gameplay"