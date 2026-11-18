namespace Interlude.Features.Rulesets

open Interlude.Features.Gameplay
open Percyqaz.Common
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.Options
open Prelude.Gameplay.Rulesets
open Interlude.UI
open Interlude.Content

module RulesetSwitcher =

    let make_dropdown (setting: Setting<RulesetSelection>) (w: DropdownWrapper) =

        let get_ruleset_groups () =
            let rulesets = Rulesets.list()
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

    let dropdown_wrapper =
        DropdownWrapper(fun d ->
            Position.BorderT(min d.Height 500.0f).Shrink(Style.PADDING, 0.0f).Translate(0.0f, -Style.PADDING)
        )

    override this.Init(parent: Widget) : unit =
        this.Add(
            AngledButton(
                (fun () -> Rulesets.GetCurrent(SelectedChart.CACHE_DATA).Ruleset.Name),
                (fun () -> this.ToggleDropdown()),
                Palette.MAIN_100
            )
                .LeanRight(false)
                .Hotkey("ruleset_switch"),
            dropdown_wrapper
        )

        base.Init(parent)

    member this.ToggleDropdown() : unit =
        RulesetSwitcher.make_dropdown setting dropdown_wrapper
