namespace Interlude.Features.Rulesets.Edit

open Percyqaz.Common
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay.Rulesets
open Interlude.UI

type EditLampsPage(ruleset: Setting<Ruleset>) =
    inherit Page()

    let container = FlowContainer.Vertical<Widget>(PAGE_ITEM_HEIGHT)

    let rec lamp_controls (i: int, l: Lamp) =
        NavigationContainer
            .Row()
            .With(
                PageButton(l.Name, fun () -> EditLampPage(ruleset, i).Show())
                    .TextColor(l.Color)
                    .Position(Position.ShrinkR(PAGE_ITEM_HEIGHT)),
                Button(
                    Icons.TRASH,
                    (fun () ->
                        ConfirmPage([ l.Name ] %> "rulesets.lamp.confirm_delete", fun () -> delete_lamp i).Show()
                    )
                )
                    .TextColor(Colors.red_accent)
                    .Position(Position.SliceR(PAGE_ITEM_HEIGHT))
            )

    and refresh () =
        container.Clear()

        for i, l in ruleset.Value.Lamps |> Seq.indexed |> Seq.rev do
            container.Add(lamp_controls(i, l))

        container.Add(Button(sprintf "%s %s" Icons.PLUS_CIRCLE %"rulesets.lamp.add", add_lamp))

    and add_lamp () =
        let new_lamp =
            {
                Name = "???"
                Color = Color.White
                Requirement = LampRequirement.ComboBreaksAtMost(System.Int32.MaxValue)
            }

        ruleset.Set({ ruleset.Value with Lamps = ruleset.Value.Lamps |> Array.append [| new_lamp |] })
        GameThread.defer refresh

    and delete_lamp (i: int) : unit =
        ruleset.Set({ ruleset.Value with Lamps = ruleset.Value.Lamps |> Array.removeAt i })
        refresh()

    override this.Content() : Widget =
        refresh()
        ScrollContainer(container).Position(Position.Shrink(PAGE_MARGIN_X, PAGE_MARGIN_Y).SliceL(PAGE_ITEM_WIDTH))

    override this.Title = %"rulesets.edit.lamps"
    override this.OnReturnFromNestedPage() : unit = refresh()
