namespace Interlude.Features.LevelSelect

open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.Data.Charts.Library
open Prelude.Data.Charts.Sorting
open Interlude.Options
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Components
open Interlude.UI.Menu
open Interlude.Features.Gameplay

type private ModeDropdown(options: (string * string) seq, label: string, setting: Setting<string>, reverse: Setting<bool>, bind: Hotkey) =
    inherit StaticContainer(NodeType.None)

    let mutable displayValue = Seq.find (fun (id, _) -> id = setting.Value) options |> snd

    override this.Init(parent: Widget) =
        this 
        |+ StylishButton(
            ( fun () -> this.ToggleDropdown() ),
            K (label + ":"),
            Style.highlight 100,
            Hotkey = bind,
            Position = Position.SliceLeft 120.0f)
        |* StylishButton(
            ( fun () -> reverse.Value <- not reverse.Value ),
            ( fun () -> sprintf "%s %s" displayValue (if reverse.Value then Icons.order_descending else Icons.order_ascending) ),
            Style.dark 100,
            Position = Position.TrimLeft 145.0f )
        base.Init parent

    member this.ToggleDropdown() =
        match this.Dropdown with
        | Some _ -> this.Dropdown <- None
        | _ ->
            let d = Dropdown.Selector options snd (fun g -> displayValue <- snd g; setting.Set (fst g)) (fun () -> this.Dropdown <- None)
            d.Position <- Position.SliceTop(d.Height + 60.0f).TrimTop(60.0f).Margin(Style.padding, 0.0f)
            d.Init this
            this.Dropdown <- Some d

    member val Dropdown : Dropdown option = None with get, set

    override this.Draw() =
        base.Draw()
        match this.Dropdown with
        | Some d -> d.Draw()
        | None -> ()

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        match this.Dropdown with
        | Some d -> d.Update(elapsedTime, moved)
        | None -> ()
    
type LibraryModeSettings() =
    inherit StaticContainer(NodeType.None)

    let group_selector = 
        ModeDropdown(
            groupBy.Keys
            |> Seq.map (fun id -> (id, Localisation.localise (sprintf "levelselect.groupby." + id))),
            "Group",
            options.ChartGroupMode |> Setting.trigger (fun _ -> LevelSelect.refresh <- true),
            options.ChartGroupReverse |> Setting.trigger (fun _ -> LevelSelect.refresh <- true),
            "group_mode"
        ).Tooltip(L"levelselect.groupby.tooltip", "group_mode")

    let manage_collections =
        StylishButton(
            (fun () -> Menu.ShowPage SelectCollectionPage.Editor),
            K (sprintf "%s %s" Icons.collections (L"levelselect.collections.name")),
            Style.main 100,
            Hotkey = "group_mode"
        ).Tooltip(L"levelselect.collections.tooltip", "group_mode")
        
    let manage_tables =
        StylishButton(
            (fun () -> Menu.ShowPage ManageTablesPage),
            K (sprintf "%s %s" Icons.edit (L"levelselect.table.name")),
            Style.main 100,
            Hotkey = "group_mode"
        ).Tooltip(L"levelselect.table.tooltip", "group_mode")

    let swap = SwapContainer(Position = { Left = 0.8f %+ 0.0f; Top = 0.0f %+ 120.0f; Right = 1.0f %+ 0.0f; Bottom = 0.0f %+ 170.0f })

    let update_swap() =
        swap.Current <-
        match options.LibraryMode.Value with
        | LibraryMode.All -> group_selector
        | LibraryMode.Collections -> manage_collections
        | LibraryMode.Table -> manage_tables

    override this.Init(parent) =
        this
        |+ StylishButton.Selector(
            sprintf "%s %s:" Icons.collections (L"levelselect.librarymode"),
            [|
                LibraryMode.All, L"levelselect.librarymode.all"
                LibraryMode.Collections, L"levelselect.librarymode.collections"
                LibraryMode.Table, L"levelselect.librarymode.table"
            |],
            options.LibraryMode |> Setting.trigger (fun _ -> LevelSelect.refresh <- true; update_swap()),
            Style.dark 100,
            Hotkey = "library_mode")
            .Tooltip(L"levelselect.librarymode.tooltip", "library_mode")
            .WithPosition { Left = 0.4f %+ 25.0f; Top = 0.0f %+ 120.0f; Right = 0.6f %- 25.0f; Bottom = 0.0f %+ 170.0f }
        
        |+ ModeDropdown(
            sortBy.Keys
            |> Seq.map (fun id -> (id, Localisation.localise (sprintf "levelselect.sortby." + id))),
            "Sort",
            options.ChartSortMode |> Setting.trigger (fun _ -> LevelSelect.refresh <- true),
            options.ChartSortReverse |> Setting.map not not |> Setting.trigger (fun _ -> LevelSelect.refresh <- true),
            "sort_mode")
            .Tooltip(L"levelselect.sortby.tooltip", "sort_mode")
            .WithPosition { Left = 0.6f %+ 0.0f; Top = 0.0f %+ 120.0f; Right = 0.8f %- 25.0f; Bottom = 0.0f %+ 170.0f }
        
        |* swap

        update_swap()
        base.Init parent
        
    
    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)

        if Chart.cacheInfo.IsSome then

            if (!|"add_to_collection").Tapped() then CollectionManager.Current.quick_add(Chart.cacheInfo.Value) |> ignore
            elif (!|"remove_from_collection").Tapped() then CollectionManager.Current.quick_remove(Chart.cacheInfo.Value, Chart.context) |> ignore
            elif (!|"move_up_in_collection").Tapped() then CollectionManager.reorder_up(Chart.context)
            elif (!|"move_down_in_collection").Tapped() then CollectionManager.reorder_down(Chart.context)

            elif (!|"collections").Tapped() then Menu.ShowPage SelectCollectionPage.Editor
            elif (!|"table").Tapped() then Menu.ShowPage ManageTablesPage
            elif (!|"reverse_sort_mode").Tapped() then 
                Setting.app not options.ChartSortReverse
                LevelSelect.refresh <- true
            elif (!|"reverse_group_mode").Tapped() then 
                Setting.app not options.ChartGroupReverse
                LevelSelect.refresh <- true