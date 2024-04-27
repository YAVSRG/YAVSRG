namespace Interlude.Features

open Percyqaz.Common
open Percyqaz.Flux.UI
open Interlude.UI

module Rulesets = Interlude.Content.Rulesets

// todo: move this UI folder and put a ruleset editor in the options menu rulesets page
module Rulesets =

    type QuickSwitcher(setting: Setting<string>) =
        inherit Container(NodeType.None)

        let dropdown_wrapper = DropdownWrapper(fun d -> Position.BorderTop(min d.Height 500.0f).Margin(Style.PADDING, 0.0f))

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
            dropdown_wrapper.Toggle(fun () ->
                let rulesets = Rulesets.list ()
                Dropdown
                    {
                        Items = rulesets |> Seq.map (fun (id, rs) -> id, rs.Name)
                        ColorFunc = K Colors.text
                        OnClose = dropdown_wrapper.Dismiss
                        Setting = setting
                    }
            )