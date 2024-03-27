namespace Interlude.Features.LevelSelect

open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.Data.Library.Sorting
open Interlude.Options
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Features.Gameplay
open Interlude.Features.Collections
open Interlude.Features.Tables

type private ModeDropdown
    (options: (string * string) seq, label: string, setting: Setting<string>, reverse: Setting<bool>, bind: Hotkey) =
    inherit Container(NodeType.None)

    let mutable display_value =
        Seq.find (fun (id, _) -> id = setting.Value) options |> snd

    override this.Init(parent: Widget) =
        this
        |+ StylishButton(
            (fun () -> this.ToggleDropdown()),
            K(label + ":"),
            !%Palette.HIGHLIGHT_100,
            Hotkey = bind,
            Position = Position.SliceLeft 120.0f
        )
        |* StylishButton(
            (fun () -> reverse.Value <- not reverse.Value),
            (fun () ->
                sprintf
                    "%s %s"
                    display_value
                    (if reverse.Value then
                         Icons.CHEVRONS_DOWN
                     else
                         Icons.CHEVRONS_UP)
            ),
            !%Palette.DARK_100,
            Position = Position.TrimLeft 145.0f
        )

        base.Init parent

    member this.ToggleDropdown() =
        match this.Dropdown with
        | Some _ -> this.Dropdown <- None
        | _ ->
            let d =
                Dropdown
                    {
                        Items = options
                        ColorFunc = K Colors.text
                        OnClose = fun () -> this.Dropdown <- None
                        Setting =
                            setting
                            |> Setting.trigger (fun v ->
                                display_value <- Seq.find (fun (id, _) -> id = v) options |> snd
                            )
                    }

            d.Position <- Position.SliceTop(d.Height + 60.0f).TrimTop(60.0f).Margin(Style.PADDING, 0.0f)
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

type LibraryModeSettings() =
    inherit Container(NodeType.None)

    let group_selector =
        ModeDropdown(
            grouping_modes.Keys
            |> Seq.map (fun id -> (id, Localisation.localise (sprintf "levelselect.groupby." + id))),
            "Group",
            options.ChartGroupMode |> Setting.trigger (ignore >> LevelSelect.refresh_all),
            options.ChartGroupReverse |> Setting.trigger (ignore >> LevelSelect.refresh_all),
            "group_mode"
        )
            .Tooltip(
                Tooltip
                    .Info("levelselect.groupby", "group_mode")
                    .Hotkey(%"levelselect.groupby.reverse_hint", "reverse_group_mode")
            )

    let manage_collections =
        StylishButton(
            (fun () -> Menu.ShowPage ManageCollectionsPage),
            K(sprintf "%s %s" Icons.FOLDER (%"levelselect.collections.name")),
            !%Palette.MAIN_100,
            Hotkey = "group_mode"
        )
            .Tooltip(Tooltip.Info("levelselect.collections", "group_mode"))

    let manage_tables =
        StylishButton(
            (fun () -> ManageTablesPage(LevelSelect.refresh_all).Show()),
            K(sprintf "%s %s" Icons.EDIT_2 (%"levelselect.table.name")),
            !%Palette.MAIN_100,
            Hotkey = "group_mode"
        )
            .Tooltip(Tooltip.Info("levelselect.table", "group_mode"))

    let swap =
        SwapContainer(
            Position =
                {
                    Left = 0.8f %+ 0.0f
                    Top = 0.0f %+ 120.0f
                    Right = 1.0f %+ 0.0f
                    Bottom = 0.0f %+ 170.0f
                }
        )

    let update_swap () =
        swap.Current <-
            match options.LibraryMode.Value with
            | LibraryMode.All -> group_selector
            | LibraryMode.Collections -> manage_collections
            | LibraryMode.Table -> manage_tables

    override this.Init(parent) =
        this
        |+ StylishButton
            .Selector(
                sprintf "%s %s:" Icons.FOLDER (%"levelselect.librarymode"),
                [|
                    LibraryMode.All, %"levelselect.librarymode.all"
                    LibraryMode.Collections, %"levelselect.librarymode.collections"
                    LibraryMode.Table, %"levelselect.librarymode.table"
                |],
                options.LibraryMode
                |> Setting.trigger (fun _ ->
                    LevelSelect.refresh_all ()
                    update_swap ()
                ),
                !%Palette.DARK_100,
                Hotkey = "library_mode",
                Position =
                    {
                        Left = 0.4f %+ 25.0f
                        Top = 0.0f %+ 120.0f
                        Right = 0.6f %- 25.0f
                        Bottom = 0.0f %+ 170.0f
                    }
            )
            .Tooltip(Tooltip.Info("levelselect.librarymode", "library_mode"))

        |+ ModeDropdown(
            sorting_modes.Keys
            |> Seq.map (fun id -> (id, Localisation.localise (sprintf "levelselect.sortby." + id))),
            "Sort",
            options.ChartSortMode |> Setting.trigger (ignore >> LevelSelect.refresh_all),
            options.ChartSortReverse
            |> Setting.map not not
            |> Setting.trigger (ignore >> LevelSelect.refresh_all),
            "sort_mode",
            Position =
                {
                    Left = 0.6f %+ 0.0f
                    Top = 0.0f %+ 120.0f
                    Right = 0.8f %- 25.0f
                    Bottom = 0.0f %+ 170.0f
                }
        )
            .Tooltip(
                Tooltip
                    .Info("levelselect.sortby", "sort_mode")
                    .Hotkey(%"levelselect.sortby.reverse_hint", "reverse_sort_mode")
            )

        |* swap

        update_swap ()
        base.Init parent


    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if Chart.CACHE_DATA.IsSome then
            if (%%"move_up_in_playlist").Tapped() then
                CollectionActions.reorder_up Chart.LIBRARY_CTX |> ignore // todo: play sound effect
            elif (%%"move_down_in_playlist").Tapped() then
                CollectionActions.reorder_down Chart.LIBRARY_CTX |> ignore

            elif (%%"collections").Tapped() then
                Menu.ShowPage ManageCollectionsPage
            elif (%%"table").Tapped() then
                ManageTablesPage(LevelSelect.refresh_all).Show() // todo: only need to refresh if view is table
            elif (%%"reverse_sort_mode").Tapped() then
                Setting.app not options.ChartSortReverse
                LevelSelect.refresh_all ()
            elif (%%"reverse_group_mode").Tapped() then
                Setting.app not options.ChartGroupReverse
                LevelSelect.refresh_all ()
