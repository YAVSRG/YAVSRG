namespace Interlude.Features.Import.Osu

open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Interlude.UI

type BeatmapSortingDropdown =

    static member Create
        (options: (string * string) seq, label: string, setting: Setting<string>, reverse: Setting<bool>, bind: Hotkey)
        =

        let mutable display_value =
            Seq.find (fun (id, _) -> id = setting.Value) options |> snd

        let dropdown_wrapper =
            DropdownWrapper(fun d -> Position.SliceT(d.Height + 60.0f).ShrinkT(60.0f).Shrink(Style.PADDING, 0.0f))

        let toggle_dropdown () =
            dropdown_wrapper.Toggle(fun () ->
                Dropdown
                    {
                        Items = options
                        ColorFunc = K Colors.text
                        Setting =
                            setting
                            |> Setting.trigger(fun v ->
                                display_value <- Seq.find (fun (id, _) -> id = v) options |> snd
                            )
                    }
            )

        let LABEL_AREA_SIZE = 120.0f

        NavigationContainer
            .Row()
            .WrapNavigation(false)
            .With(
                AngledButton(label + ":", toggle_dropdown, Colors.cyan.O3)
                    .Hotkey(bind)
                    .Position(Position.SliceL(LABEL_AREA_SIZE)),

                AngledButton(
                    (fun () ->
                        sprintf
                            "%s %s"
                            display_value
                            (if reverse.Value then Icons.CHEVRONS_DOWN else Icons.CHEVRONS_UP)
                    ),
                    (fun () -> reverse.Value <- not reverse.Value),
                    Colors.cyan_shadow.O3
                )
                    .LeanRight(false)
                    .Position(Position.ShrinkL(LABEL_AREA_SIZE + AngledButton.LEAN_AMOUNT)),

                dropdown_wrapper
            )
