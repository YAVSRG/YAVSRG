namespace Interlude.Features.LevelSelect

open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude.Mods
open Prelude
open Interlude.Options
open Interlude.UI
open Interlude.Features.Pacemaker
open Interlude.Features.Gameplay

type private ColumnSwapPage() =
    inherit Page()

    static let columns_setting = Setting.simple "1234123"

    member this.TrySaveChanges() =
        match ColumnSwap.parse columns_setting.Value with
        | Ok columns ->
            SelectedChart.selected_mods.Value <-
                ModState.cycle_column_swap "column_swap" columns SelectedChart.selected_mods.Value
        | Error reason -> Notifications.error (reason, "")

    override this.Content() =
        this.OnClose(this.TrySaveChanges)

        page_container()
            .With(
                PageTextEntry(%"mods.column_swap_columns", columns_setting)
                    .Help(Help.Info("mods.column_swap_columns"))
                    .Pos(0),
                PageButton(%"confirm.yes", Menu.Back)
                    .Pos(3)
            )

    override this.Title = %"mod.column_swap"

type private ModSelector(id: string, current_state: unit -> int64 option, action: unit -> unit) =
    inherit
        Container(
            NodeType.Button(fun () ->
                Style.click.Play()
                action ()
            )
        )

    static let TOP_HEIGHT = 60.0f
    static let DESCRIPTION_HEIGHT = 30.0f
    static member HEIGHT = DESCRIPTION_HEIGHT + TOP_HEIGHT

    override this.Init(parent: Widget) =
        this.Add(MouseListener().Button(this))
        base.Init parent

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Draw() =
        let state = current_state ()
        Render.rect (this.Bounds.SliceT(TOP_HEIGHT)) (if state.IsSome then Colors.pink.O3 else Colors.shadow_2.O2)

        Render.rect
            (this.Bounds.ShrinkT(TOP_HEIGHT))
            (if state.IsSome then
                 Colors.pink_shadow.O3
             else
                 Colors.shadow_1.O3)

        if state.IsSome then
            Text.fill_b (
                Style.font,
                Icons.CHECK,
                this.Bounds.SliceT(TOP_HEIGHT).ShrinkX(20.0f),
                Colors.text,
                Alignment.RIGHT
            )

        Text.fill_b (
            Style.font,
            Mods.name id state,
            this.Bounds.SliceT(TOP_HEIGHT).ShrinkX(20.0f),
            (if this.Focused then Colors.text_yellow_2 else Colors.text),
            Alignment.LEFT
        )

        Text.fill_b (
            Style.font,
            Mods.desc id state,
            this.Bounds.ShrinkT(TOP_HEIGHT - 2.0f).ShrinkX(20.0f),
            (if this.Focused then Colors.text_yellow_2 else Colors.text_subheading),
            Alignment.LEFT
        )

        base.Draw()

type private ModSelectPage(change_rate: Rate -> unit) =
    inherit Page()

    let mutable last_seen_mod_status = ModStatus.Ranked
    let mod_status () =
        match SelectedChart.WITH_MODS with
        | Some m -> last_seen_mod_status <- m.Status
        | None -> ()
        last_seen_mod_status

    override this.Content() =
        let mod_grid =
            GridFlowContainer<ModSelector>(ModSelector.HEIGHT, 3)
                .Spacing(20.0f)
                .WrapNavigation(false)
                .Pos(5, PAGE_BOTTOM - 5, PageWidth.Full)
                .With(
                    ModSelector(
                        "auto",
                        (fun _ -> if SelectedChart.autoplay then Some 0 else None),
                        (fun _ -> SelectedChart.autoplay <- not SelectedChart.autoplay)
                    )
                )
                .With(seq {
                    for id in Mods.MENU_DISPLAY_ORDER do
                        yield ModSelector(
                            id,
                            (fun _ ->
                                if SelectedChart.selected_mods.Value.ContainsKey id then
                                    Some SelectedChart.selected_mods.Value.[id]
                                else
                                    None
                            ),
                            (fun _ ->
                                if id = "column_swap" && not (SelectedChart.selected_mods.Value.ContainsKey id) then
                                    ColumnSwapPage().Show()
                                else
                                    Setting.app (ModState.cycle id) SelectedChart.selected_mods
                            )
                        )
                })
                .With(
                    ModSelector(
                        "pacemaker",
                        (fun _ -> if options.EnablePacemaker.Value then Some 0 else None),
                        (fun _ -> Setting.app not options.EnablePacemaker)
                    )
                )

        page_container()
            .With(
                PageSetting(%"gameplay.rate",
                    Slider(
                        SelectedChart.rate
                        |> Setting.map id (fun v -> round (v / 0.05f<rate>) * 0.05f<rate>)
                        |> Setting.uom,
                        Format = sprintf "%.02fx"
                    )
                )
                    .Help(
                        Help.Info("gameplay.rate")
                            .Hotkey(%"levelselect.selected_mods.uprate.hint", "uprate")
                            .Hotkey(%"levelselect.selected_mods.downrate.hint", "downrate")
                    )
                    .Pos(0),
                Text([(%%"uprate").ToString(); (%%"downrate").ToString()] %> "gameplay.rate.hotkey_hint_i")
                    .Color(Colors.text_subheading)
                    .Align(Alignment.LEFT)
                    .Position(page_position(2, 1, PageWidth.Full).ShrinkL(PAGE_LABEL_WIDTH)),
                Text(%"gameplay.rate.hotkey_hint_ii")
                    .Color(Colors.text_subheading)
                    .Align(Alignment.LEFT)
                    .Position(page_position(3, 1, PageWidth.Full).ShrinkL(PAGE_LABEL_WIDTH)),

                mod_grid,

                PageButton(%"gameplay.pacemaker", fun () -> PacemakerOptionsPage().Show())
                    .Icon(Icons.FLAG)
                    .Help(Help.Info("gameplay.pacemaker"))
                    .Pos(19),

                PageSetting(%"mods.mod_status",
                    Text(fun () ->
                        match mod_status() with
                        | ModStatus.Ranked -> %"mods.mod_status.ranked"
                        | ModStatus.Unranked -> %"mods.mod_status.unranked"
                        | ModStatus.Offline -> %"mods.mod_status.offline"
                        | ModStatus.Unstored -> %"mods.mod_status.unstored"
                     )
                        .Color(fun () ->
                            match mod_status() with
                            | ModStatus.Ranked -> Colors.text_green_2
                            | ModStatus.Unranked -> Colors.text_yellow_2
                            | ModStatus.Offline -> Colors.text_yellow_2
                            | ModStatus.Unstored -> Colors.text_red
                        )
                        .Align(Alignment.LEFT)
                )
                    .Pos(21)
            )

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if (%%"autoplay").Pressed() then
            SelectedChart.autoplay <- not SelectedChart.autoplay
            Style.click.Play()
        elif (%%"reset_mods").Pressed() then
            SelectedChart.autoplay <- false
            SelectedChart.selected_mods.Set Map.empty
            Style.click.Play()
        elif (%%"mods").Pressed() then
            Menu.Back()
        else
            SelectedChart.change_rate_hotkeys change_rate

    override this.Title = sprintf "%s %s" Icons.ZAP (%"mods")

type ModSelect(change_rate: Rate -> unit) =
    inherit
        AngledButton(
            sprintf "%s %s" Icons.ZAP (%"levelselect.mods"),
            (fun () -> ModSelectPage(change_rate).Show()),
            (fun () -> Palette.color (100, 0.5f, 0.0f)),
            Hotkey = "mods"
        )

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if (%%"autoplay").Pressed() then
            SelectedChart.autoplay <- not SelectedChart.autoplay
            Style.click.Play()
        elif (%%"reset_mods").Pressed() then
            SelectedChart.autoplay <- false
            SelectedChart.selected_mods.Set Map.empty
            Style.click.Play()
        else
            SelectedChart.change_rate_hotkeys change_rate