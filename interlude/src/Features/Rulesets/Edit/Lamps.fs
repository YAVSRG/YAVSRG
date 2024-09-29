namespace Interlude.Features.Rulesets.Edit

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay
open Interlude.UI

type EditLampPage(ruleset: Setting<Ruleset>, id: int) =
    inherit Page()

    let lamp = ruleset.Value.Grading.Lamps.[id]
    let name = Setting.simple lamp.Name
    let color = Setting.simple lamp.Color
    let judgement_type = Setting.simple lamp.Judgement
    let judgement_threshold = Setting.simple lamp.JudgementThreshold

    override this.Content() =
        page_container()
        |+ PageTextEntry(%"rulesets.lamp.name", name)
            .Pos(0)
        |+ PageSetting(%"rulesets.lamp.color", ColorPicker(color, false))
            .Pos(2, 3)
        |+ PageSetting(%"rulesets.lamp.requirement",
            NavigationContainer.Row()
            |+ SelectDropdown(
                seq {
                    for i, j in ruleset.Value.Judgements |> Array.indexed do
                        yield i, j.Name
                    yield -1, "Combo breaks"
                } |> Array.ofSeq,
                judgement_type,
                Position = { Position.DEFAULT with Right = 0.5f %- 100.0f }
            )
            |+ Text("<=")
            |+ Selector(
                [|0, "0"; 1, "1"; 9, "9"; 99, "99"|],
                judgement_threshold,
                Position = { Position.DEFAULT with Left = 0.5f %+ 100.0f }
            )
        )
            .Pos(5)
        :> Widget

    override this.Title = lamp.Name
    override this.OnClose() =
        let new_lamps = ruleset.Value.Grading.Lamps |> Array.copy
        new_lamps.[id] <- { Name = name.Value.Trim(); Color = color.Value; Judgement = judgement_type.Value; JudgementThreshold = judgement_threshold.Value }
        let new_grading = 
            { ruleset.Value.Grading with
                Lamps = 
                    new_lamps 
                    |> Array.sortByDescending (fun l -> 
                        if l.Judgement = -1 then
                            System.Int32.MaxValue, l.JudgementThreshold 
                        else
                            l.Judgement, l.JudgementThreshold
                    ) 
            }
        ruleset.Set { ruleset.Value with Grading = new_grading }

type EditLampsPage(ruleset: Setting<Ruleset>) =
    inherit Page()

    let container = FlowContainer.Vertical<Widget>(PRETTYHEIGHT)

    let rec lamp_controls (i: int, l: Lamp) =
        NavigationContainer.Row()
        |+ ColoredButton(l.Name, l.Color, (fun () -> EditLampPage(ruleset, i).Show()), Position = Position.ShrinkR PRETTYHEIGHT)
        |+ Button(
            Icons.TRASH,
            (fun () -> 
                ConfirmPage(
                    [l.Name] %> "rulesets.lamp.confirm_delete",
                    fun () -> delete_lamp i
                ).Show()
            ),
            Position = Position.SliceR PRETTYHEIGHT
        )

    and refresh() =
        container.Clear()
        for i, l in ruleset.Value.Grading.Lamps |> Seq.indexed |> Seq.rev do
            container.Add (lamp_controls (i, l))
        container.Add <| Button(sprintf "%s %s" Icons.PLUS_CIRCLE %"rulesets.lamp.add", add_lamp)

    and add_lamp() =
        let new_lamp =
            {
                Name = "???"
                Color = Color.White
                Judgement = -1
                JudgementThreshold = System.Int32.MaxValue
            }
        let new_grading = 
            { ruleset.Value.Grading with 
                Lamps = ruleset.Value.Grading.Lamps |> Array.append [| new_lamp |] 
            }
        ruleset.Set { ruleset.Value with Grading = new_grading }
        defer refresh

    and delete_lamp(i: int) : unit =
        let new_grading = { ruleset.Value.Grading with Lamps = ruleset.Value.Grading.Lamps |> Array.removeAt i }
        ruleset.Set { ruleset.Value with Grading = new_grading }
        refresh()

    override this.Content() =
        refresh()
        ScrollContainer(container, Position = Position.Shrink(PRETTY_MARGIN_X, PRETTY_MARGIN_Y).SliceL(PRETTYWIDTH))

    override this.Title = %"rulesets.edit.lamps"
    override this.OnClose() = ()
    override this.OnReturnFromNestedPage() = refresh()