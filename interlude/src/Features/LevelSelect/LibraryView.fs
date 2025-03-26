namespace Interlude.Features.LevelSelect

open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.Library
open Interlude.Options
open Interlude.UI
open Interlude.Features.Gameplay
open Interlude.Features.Collections
open Interlude.Features.Tables

type private ModeDropdown
    (options: (string * string) seq, label: string, setting: Setting<string>, reverse: Setting<bool>, bind: Hotkey) =
    inherit Container(NodeType.None)

    let LEFT_PERCENT = 0.4f

    let mutable display_value =
        Seq.find (fun (id, _) -> id = setting.Value) options |> snd

    let dropdown_wrapper = DropdownWrapper(fun d -> Position.SliceT(d.Height + 60.0f).ShrinkT(60.0f).Shrink(Style.PADDING, 0.0f))

    override this.Init(parent: Widget) =
        this
            .Add(
                AngledButton(
                    label + ":",
                    (fun () -> this.ToggleDropdown()),
                    Palette.HIGHLIGHT_100
                )
                    .Hotkey(bind)
                    .Position(Position.SlicePercentL(LEFT_PERCENT)),

                AngledButton(
                    (fun () ->
                        sprintf
                            "%s %s"
                            display_value
                            (if reverse.Value then
                                 Icons.CHEVRONS_DOWN
                             else
                                 Icons.CHEVRONS_UP)
                    ),
                    (fun () -> reverse.Value <- not reverse.Value),
                    Palette.DARK_100
                )
                    .Position(Position.ShrinkPercentL(LEFT_PERCENT).ShrinkL(AngledButton.LEAN_AMOUNT)),

                dropdown_wrapper
            )

        base.Init parent

    member this.ToggleDropdown() =
        dropdown_wrapper.Toggle(fun () ->
            Dropdown
                {
                    Items = options
                    ColorFunc = K Colors.text
                    Setting =
                        setting
                        |> Setting.trigger (fun v ->
                            display_value <- Seq.find (fun (id, _) -> id = v) options |> snd
                        )
                }
        )

type LibraryViewControls() =
    inherit Container(NodeType.None)

    let OPTIONS_BUTTON_WIDTH = 60.0f

    override this.Init(parent) =
        this
        |+ AngledButton(
            Icons.SETTINGS,
            (fun () -> LevelSelectOptionsPage().Show()),
            Palette.DARK_100
        )
            .Hotkey("level_select_options")
            .Position(Position.SliceL(AngledButton.LEAN_AMOUNT, OPTIONS_BUTTON_WIDTH))

        |+ ModeDropdown(
            Sorting.modes.Keys
            |> Seq.map (fun id -> (id, Localisation.localise (sprintf "levelselect.sortby." + id))),
            "Sort",
            options.ChartSortMode |> Setting.trigger (ignore >> LevelSelect.refresh_all),
            options.ChartSortReverse
            |> Setting.map not not
            |> Setting.trigger (ignore >> LevelSelect.refresh_all),
            "sort_mode"
        )
            .Position(
                Position
                    .ShrinkL(OPTIONS_BUTTON_WIDTH + AngledButton.LEAN_AMOUNT * 2.0f)
                    .GridX(1, 2, AngledButton.LEAN_AMOUNT)
            )
            .Help(
                Help
                    .Info("levelselect.sortby", "sort_mode")
                    .Hotkey(%"levelselect.sortby.reverse_hint", "reverse_sort_mode")
            )

        |* ModeDropdown(
            Grouping.modes.Keys
            |> Seq.map (fun id -> (id, Localisation.localise (sprintf "levelselect.groupby." + id))),
            "Group",
            options.ChartGroupMode |> Setting.trigger (ignore >> LevelSelect.refresh_all),
            options.ChartGroupReverse |> Setting.trigger (ignore >> LevelSelect.refresh_all),
            "group_mode"
        )
            .Position(
                Position
                    .ShrinkL(OPTIONS_BUTTON_WIDTH + AngledButton.LEAN_AMOUNT * 2.0f)
                    .GridX(2, 2, AngledButton.LEAN_AMOUNT)
            )
            .Help(
                Help
                    .Info("levelselect.groupby", "group_mode")
                    .Hotkey(%"levelselect.groupby.reverse_hint", "reverse_group_mode")
            )

        base.Init parent

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if SelectedChart.CACHE_DATA.IsSome then
            if (%%"move_up_in_playlist").Pressed() then
                CollectionActions.reorder_up SelectedChart.LIBRARY_CTX |> ignore // todo: play sound effect
            elif (%%"move_down_in_playlist").Pressed() then
                CollectionActions.reorder_down SelectedChart.LIBRARY_CTX |> ignore
            elif (%%"like").Pressed() then
                CollectionActions.toggle_liked SelectedChart.CACHE_DATA.Value

            //elif (%%"skip").Pressed() then
            //    FiltersPage().Show()

            elif (%%"collections").Pressed() then
                ManageCollectionsPage().Show()
            elif (%%"table").Pressed() then
                SelectTablePage(LevelSelect.refresh_all).Show()
            elif (%%"reverse_sort_mode").Pressed() then
                Setting.app not options.ChartSortReverse
                LevelSelect.refresh_all ()
            elif (%%"reverse_group_mode").Pressed() then
                Setting.app not options.ChartGroupReverse
                LevelSelect.refresh_all ()