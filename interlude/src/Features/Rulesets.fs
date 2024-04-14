namespace Interlude.Features

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Interlude.Options
open Interlude.UI
open Interlude.UI.Menu

module Rulesets = Interlude.Content.Rulesets

// todo: move this UI folder and put a ruleset editor in the options menu rulesets page
module Rulesets =

    type QuickSwitcher(setting: Setting<string>) =
        inherit Container(NodeType.None)

        override this.Init(parent: Widget) =
            this
            |* StylishButton(
                (fun () -> this.ToggleDropdown()),
                (fun () -> Rulesets.current.Name),
                !%Palette.MAIN_100,
                TiltRight = false,
                Hotkey = "ruleset_switch"
            )

            base.Init parent

        member this.ToggleDropdown() =
            match this.Dropdown with
            | Some _ -> this.Dropdown <- None
            | _ ->
                let rulesets = Rulesets.list ()

                let d =
                    Dropdown
                        {
                            Items = rulesets |> Seq.map (fun (id, rs) -> id, rs.Name)
                            ColorFunc = K Colors.text
                            OnClose = fun () -> this.Dropdown <- None
                            Setting = setting
                        }

                d.Position <-
                    Position
                        .BorderTop(min d.Height 500.0f)
                        .Margin(Style.PADDING, 0.0f)

                d.Init this
                this.Dropdown <- Some d

        member val Dropdown: Dropdown<string> option = None with get, set

        override this.Draw() =
            base.Draw()

            match this.Dropdown with
            | Some d -> d.Draw()
            | None -> ()

        override this.Update(elapsed_ms, moved) =
            base.Update(elapsed_ms, moved)

            match this.Dropdown with
            | Some d -> d.Update(elapsed_ms, moved)
            | None -> ()