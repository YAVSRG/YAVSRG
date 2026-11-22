namespace Interlude.Features.LevelSelect

open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Interlude.UI

type private ModeDropdown
    (options: (string * string) seq, label: string, setting: Setting<string>, reverse: Setting<bool>, bind: Hotkey) =
    inherit Container(NodeType.None)

    let LEFT_PERCENT = 0.4f

    let mutable display_value =
        Seq.find (fun (id, _) -> id = setting.Value) options |> snd

    let dropdown_wrapper =
        DropdownWrapper(fun d -> Position.SliceT(d.Height + 60.0f).ShrinkT(60.0f).Shrink(Style.PADDING, 0.0f))

    override this.Init(parent: Widget) : unit =
        this.Add(
            AngledButton(label + ":", (fun () -> this.ToggleDropdown()), Palette.HIGHLIGHT_100)
                .Hotkey(bind)
                .Position(Position.SlicePercentL(LEFT_PERCENT)),

            AngledButton(
                (fun () ->
                    sprintf "%s %s" display_value (if reverse.Value then Icons.CHEVRONS_DOWN else Icons.CHEVRONS_UP)
                ),
                (fun () -> reverse.Value <- not reverse.Value),
                Palette.DARK_100
            )
                .Position(Position.ShrinkPercentL(LEFT_PERCENT).ShrinkL(AngledButton.LEAN_AMOUNT)),

            dropdown_wrapper
        )

        base.Init(parent)

    member this.ToggleDropdown() : unit =
        dropdown_wrapper.Toggle(fun () ->
            Dropdown
                {
                    Items = options
                    ColorFunc = K Colors.text
                    Setting =
                        setting
                        |> Setting.trigger(fun v -> display_value <- Seq.find (fun (id, _) -> id = v) options |> snd)
                }
        )
