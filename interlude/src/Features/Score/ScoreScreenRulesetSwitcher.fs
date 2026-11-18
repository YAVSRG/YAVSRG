namespace Interlude.Features.Score

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.Options
open Prelude.Gameplay.Rulesets
open Prelude.Data.UserDatabase
open Interlude.Content
open Interlude.UI
open Interlude.Features.Rulesets

type ScoreScreenRulesetSwitcher
    (setting: Setting<RulesetSelection>, set_ruleset_direct: RulesetInfo -> unit, score_info: ScoreInfo) =
    inherit Container(NodeType.None)

    let MAX_HEIGHT = 700.0f
    let mutable dropdown_height = MAX_HEIGHT

    let dropdown_wrapper =
        DropdownWrapper(fun d ->
            dropdown_height <- min d.Height MAX_HEIGHT
            Position.BorderT(dropdown_height).ShrinkX(Style.PADDING).TranslateY(-10.0f)
        )

    let switch_to_native_ruleset () =
        match Ruleset.GetNativeRuleset(score_info.ChartMeta.Origins) with
        | Some ruleset_info ->
            // Use user's version (custom colors, names, etc) if available
            Rulesets.by_hash(ruleset_info.Hash) |> Option.defaultValue ruleset_info |> set_ruleset_direct
            Style.click.Play()
        | None ->
            Notifications.system_feedback(
                Icons.HELP_CIRCLE,
                %"notification.no_native_ruleset.title",
                %"notification.no_native_ruleset.body"
            )

    override this.Init(parent: Widget) : unit =
        this.Add(
            InlaidButton((fun () -> score_info.Ruleset.Name), (fun () -> this.ToggleDropdown()))
                .Hotkey("ruleset_switch")
                .HoverText(%"score.switch_ruleset"),

            HotkeyListener("native_ruleset", switch_to_native_ruleset),

            dropdown_wrapper
        )

        base.Init(parent)

    override this.Draw() : unit =
        base.Draw()

        if dropdown_wrapper.Active && not score_info.ChartMeta.Origins.IsEmpty then
            Text.draw_aligned_b(
                Style.font,
                sprintf "%O: %s" %%"native_ruleset" %"ruleset.use_native",
                20.0f,
                this.Bounds.Right - 10.0f,
                this.Bounds.Top - dropdown_height - 50.0f,
                Colors.text_cyan_2,
                Alignment.RIGHT
            )

    member this.ToggleDropdown() : unit =
        RulesetSwitcher.make_dropdown setting dropdown_wrapper
