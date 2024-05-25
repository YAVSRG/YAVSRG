namespace Interlude.Features

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Interlude.UI

module Rulesets = Interlude.Content.Rulesets

// todo: move this UI folder and put a ruleset editor in the options menu rulesets page
module Rulesets =

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
                        if groups.Length = 1 && (snd groups.[0]).Length = 1 && Screen.current_type <> Screen.Type.Score then
                            yield (
                                (fun () -> 
                                    Import.ImportScreen.switch_to_rulesets()
                                    Screen.change Screen.Type.Import Transitions.Flags.Default |> ignore
                                ),
                                %"rulesets.get_more_rulesets"
                            )
                        for name, items in groups do
                            if items.Length < 3 then
                                for (id, rs) in items do
                                    yield ((fun () -> setting.Set id), rs.Name)
                            else
                                let inner_items = items |> Array.map (fun (id, rs) -> (fun () -> setting.Set id), rs.Name)
                                let inner_dropdown = DropdownMenu { Items = inner_items }
                                yield ((fun () -> defer (fun () -> w.Show inner_dropdown)), name + " >")
                    }
                DropdownMenu
                    {
                        Items = dropdown_items
                    }
            )

    type QuickSwitcher(setting: Setting<string>) =
        inherit Container(NodeType.None)

        let dropdown_wrapper = DropdownWrapper(fun d -> Position.BorderTop(min d.Height 500.0f).Margin(Style.PADDING, 0.0f).Translate(0.0f, -Style.PADDING))

        override this.Init(parent: Widget) =
            this
            |+ StylishButton(
                (fun () -> this.ToggleDropdown()),
                (fun () -> Rulesets.current.Name),
                !%Palette.MAIN_100,
                TiltRight = false,
                Hotkey = "ruleset_switch"
            )
            |* dropdown_wrapper

            base.Init parent

        member this.ToggleDropdown() =
            make_dropdown setting dropdown_wrapper