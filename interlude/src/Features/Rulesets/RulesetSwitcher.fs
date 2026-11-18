namespace Interlude.Features.Rulesets

open Interlude.Features.Gameplay
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.Options
open Prelude.Gameplay.Rulesets
open Interlude.UI
open Interlude.Content

module RulesetSwitcher =

    let make_dropdown (setting: Setting<RulesetSelection>) (w: DropdownWrapper) =

        let get_ruleset_groups () =
            let rulesets = rulesets.ToList()
            let remove_bracket = System.Text.RegularExpressions.Regex("\\(.+?\\)")

            rulesets
            |> Seq.groupBy(fun (id, _) -> id.Split("-").[0].Trim())
            |> Seq.map(fun (_, grouped) ->
                let arr =
                    grouped |> Seq.map(fun (id, ruleset_info) -> (id, ruleset_info.Ruleset)) |> Array.ofSeq

                let group_name = remove_bracket.Replace((snd arr.[0]).Name, "").Trim()
                group_name, arr
            )
            |> Array.ofSeq

        let ruleset_button (id: string, ruleset: Ruleset) =
            (fun () -> setting.Set(FileId(id))), ruleset.Name

        let nested_ruleset_dropdown (name, items) =
            let inner_items = items |> Array.map ruleset_button
            let inner_dropdown = DropdownMenu { Items = inner_items }

            ((fun () ->
                GameThread.defer(fun () ->
                    w.Show(inner_dropdown)
                    w.OnClose <- Selection.unclamp
                    Selection.clamp_to inner_dropdown
                )
             ),
             name + " >")

        w.Toggle(fun () ->

            let groups = get_ruleset_groups()

            let dropdown_items =
                seq {
                    yield ((fun () -> SelectRulesetPage().Show()), %"rulesets")

                    for name, items in groups do
                        if items.Length < 3 then
                            yield! items |> Seq.map ruleset_button
                        else
                            yield nested_ruleset_dropdown(name, items)
                }

            DropdownMenu { Items = dropdown_items }
        )

type RulesetSwitcher(setting: Setting<RulesetSelection>) =
    inherit Container(NodeType.None)

    let MAX_HEIGHT = 700.0f
    let mutable dropdown_height = MAX_HEIGHT

    let dropdown_wrapper =
        DropdownWrapper(fun d ->
            dropdown_height <- min d.Height MAX_HEIGHT
            Position.BorderT(min d.Height MAX_HEIGHT).Shrink(Style.PADDING, 0.0f).Translate(0.0f, -Style.PADDING)
        )

    let switch_to_native_ruleset () =
        match SelectedChart.CACHE_DATA with
        | Some chart_meta ->
            match Ruleset.GetNativeRulesetType(chart_meta.Origins) with
            | Some(NativeRuleset.Wife3) -> rulesets.EnsureInstalledAndSelect(WIFE3_J4)
            | Some(NativeRuleset.Quaver) -> rulesets.EnsureInstalledAndSelect(QUAVER_STANDARD)
            | Some(NativeRuleset.OsuMania(od)) -> rulesets.Selected.Set(OsuAutomatic(od))
            | None ->
                Notifications.system_feedback(
                    Icons.HELP_CIRCLE,
                    %"notification.no_native_ruleset.title",
                    %"notification.no_native_ruleset.body"
                )
        | None -> ()

    override this.Init(parent: Widget) : unit =
        this.Add(
            AngledButton(
                (fun () -> rulesets.GetCurrent(SelectedChart.CACHE_DATA).Ruleset.Name),
                (fun () -> this.ToggleDropdown()),
                Palette.MAIN_100
            )
                .LeanRight(false)
                .Hotkey("ruleset_switch"),
            HotkeyListener("native_ruleset", switch_to_native_ruleset),
            dropdown_wrapper
        )

        base.Init(parent)

    override this.Draw() =
        base.Draw()

        let show_native_hint =
            dropdown_wrapper.Active
            && SelectedChart.CACHE_DATA.IsSome
            && not SelectedChart.CACHE_DATA.Value.Origins.IsEmpty

        if show_native_hint then
            Text.draw_aligned_b(
                Style.font,
                sprintf "%O: %s" %%"native_ruleset" %"ruleset.use_native",
                20.0f,
                this.Bounds.CenterX,
                this.Bounds.Top - dropdown_height - 50.0f,
                Colors.text_cyan_2,
                Alignment.CENTER
            )

    member this.ToggleDropdown() : unit =
        RulesetSwitcher.make_dropdown setting dropdown_wrapper
