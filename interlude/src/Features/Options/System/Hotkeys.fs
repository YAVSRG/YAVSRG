namespace Interlude.Features.OptionsMenu.SystemSettings

open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Prelude
open Interlude.UI

type private Keybinder(hotkey: Hotkey) as this =
    inherit Container(NodeType.FocusTrap)

    let set = fun v -> Hotkeys.set hotkey v

    let rec input_callback (b) =
        match b with
        | Bind.Key(k, (ctrl, _, shift)) ->
            set <| Bind.Key(k, (ctrl, false, shift))
            this.Focus false
            Style.key.Play()
        | _ -> Input.listen_to_next_key input_callback

    do
        this
        |+ Text(
            (fun () -> (%%hotkey).ToString()),
            Color =
                (fun () ->
                    (if this.Selected then Colors.pink_accent
                        elif this.Focused then Colors.yellow_accent
                        else Colors.white),
                    Colors.shadow_1
                ),
            Align = Alignment.LEFT,
            Position = Position.ShrinkL 20.0f
        )
        |* MouseListener().Button(this)

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.OnSelected(by_mouse: bool) =
        base.OnSelected by_mouse
        Style.click.Play()
        Input.listen_to_next_key input_callback

    override this.OnDeselected(by_mouse: bool) =
        base.OnDeselected by_mouse
        Input.remove_listener ()

type HotkeysPage() =
    inherit Page()

    override this.Content() =
        let container = FlowContainer.Vertical<Widget>(PAGE_ITEM_HEIGHT)

        let scroll_container =
            ScrollContainer(container, Position = Position.Shrink(PAGE_MARGIN_X, PAGE_MARGIN_Y).SliceL(PAGE_ITEM_WIDTH))

        let search_box =
            { new SearchBox(
                    Setting.simple "",
                    (fun query ->
                        if query = "" then
                            container.Filter <- K true
                        else
                            let query = query
                            container.Filter <-
                            function
                            | :? PageSetting as p -> p.Label.Contains(query, System.StringComparison.InvariantCultureIgnoreCase)
                            | _ -> false
                    ),
                    Position = Position.SliceT(40.0f, 60.0f).Shrink(PAGE_MARGIN_X, 0.0f).SliceR(500.0f),
                    Fill = K Colors.cyan.O3,
                    Border = K Colors.cyan_accent,
                    TextColor = K Colors.text_cyan) with
                override this.OnFocus by_mouse =
                    base.OnFocus by_mouse
                    if not by_mouse then GameThread.defer (fun () -> this.Select false)
            }

        let hotkey_editor (hotkey: Hotkey) =
            NavigationContainer.Row()
            |+ Keybinder(hotkey, Position = Position.ShrinkR PAGE_ITEM_HEIGHT)
            |+ Button(Icons.REFRESH_CCW, (fun () -> Hotkeys.reset hotkey), Position = Position.SliceR PAGE_ITEM_HEIGHT)

        container.Add(
            PageButton(
                %"system.hotkeys.reset",
                (fun () -> ConfirmPage(%"system.hotkeys.reset.confirm", Hotkeys.reset_all).Show()),
                Icon = Icons.REFRESH_CCW
            )
        )

        for hk in Hotkeys.hotkeys.Keys do
            if hk <> "none" then
                container.Add(
                    PageSetting(%(sprintf "hotkeys.%s" hk), hotkey_editor hk)
                        .Help(Help.Info(sprintf "hotkeys.%s" hk))
                )

        NavigationContainer.Column()
        |+ scroll_container
        |+ search_box
        :> Widget

    override this.Title = %"system.hotkeys"
    override this.OnClose() = ()