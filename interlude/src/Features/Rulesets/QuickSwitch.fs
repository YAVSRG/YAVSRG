namespace Interlude.Features.Rulesets

open Percyqaz.Common
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay.Rulesets
open Interlude.UI
open Interlude.Content

module RulesetSwitcher =

    let make_dropdown (setting: Setting<string>) (w: DropdownWrapper) =
        w.Toggle(fun () ->
            let rulesets = Rulesets.list ()
            let remove_bracket = System.Text.RegularExpressions.Regex("\\(.+?\\)")
            let groups =
                rulesets
                |> Seq.groupBy(fun (id, _) -> id.Split("-").[0].Trim())
                |> Seq.map (fun (_, grouped) ->
                    let arr = grouped |> Array.ofSeq
                    let group_name = remove_bracket.Replace((snd arr.[0]).Name, "").Trim()
                    group_name, arr
                )
                |> Array.ofSeq
            let dropdown_items =
                seq {
                    yield (
                        (fun () -> SelectRulesetPage().Show()),
                        %"rulesets"
                    )
                    for name, items in groups do
                        if items.Length < 3 then
                            for (id, rs) in items do
                                yield ((fun () -> setting.Set id), rs.Name)
                        else
                            let inner_items = items |> Array.map (fun (id, rs) -> (fun () -> setting.Set id), rs.Name)
                            let inner_dropdown = DropdownMenu { Items = inner_items }
                            yield (
                                (fun () ->
                                    GameThread.defer (fun () ->
                                        w.Show inner_dropdown
                                        w.OnClose <- Selection.unclamp
                                        Selection.clamp_to inner_dropdown
                                    )
                                ),
                                name + " >"
                            )
                }
            DropdownMenu
                {
                    Items = dropdown_items
                }
        )

type RulesetSwitcher(setting: Setting<string>) =
    inherit Container(NodeType.None)

    let dropdown_wrapper = DropdownWrapper(fun d -> Position.BorderT(min d.Height 500.0f).Shrink(Style.PADDING, 0.0f).Translate(0.0f, -Style.PADDING))

    override this.Init(parent: Widget) =
        this
            .Add(
                AngledButton(
                    (fun () -> Rulesets.current.Name),
                    (fun () -> this.ToggleDropdown()),
                    Palette.MAIN_100
                )
                    .LeanRight(false)
                    .Hotkey("ruleset_switch"),
                dropdown_wrapper
            )

        base.Init parent

    member this.ToggleDropdown() =
        RulesetSwitcher.make_dropdown setting dropdown_wrapper