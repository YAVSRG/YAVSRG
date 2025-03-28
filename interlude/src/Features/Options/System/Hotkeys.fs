namespace Interlude.Features.OptionsMenu.SystemSettings

open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude
open Interlude.UI

type private Keybinder(hotkey: Hotkey) as this =
    inherit Container(NodeType.FocusTrap)

    let set = fun v -> Hotkeys.set hotkey v

    let rec input_callback (b: Bind) : unit =
        match b with
        | Bind.Key(k, (ctrl, _, shift)) ->
            set <| Bind.Key(k, (ctrl, false, shift))
            this.Focus false
            Style.key.Play()
        | _ -> Input.listen_to_next_key input_callback

    override this.Init(parent: Widget) : unit =
        this
            .Add(
                Text(fun () -> (%%hotkey).ToString())
                    .Color(fun () ->
                        (
                            if this.Selected then Colors.pink_accent
                            elif this.Focused then Colors.yellow_accent
                            else Colors.white
                        ),
                        Colors.shadow_1
                    )
                    .Align(Alignment.LEFT)
                    .Position(Position.ShrinkL(20.0f)),

                MouseListener().Button(this)
            )

        base.Init parent

    override this.OnFocus(by_mouse: bool) : unit =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.OnSelected(by_mouse: bool) : unit =
        base.OnSelected by_mouse
        Style.click.Play()
        Input.listen_to_next_key input_callback

    override this.OnDeselected(by_mouse: bool) : unit =
        base.OnDeselected by_mouse
        Input.remove_listener ()

type HotkeysPage() =
    inherit Page()

    override this.Content() =
        let container = FlowContainer.Vertical<Widget>(PAGE_ITEM_HEIGHT)

        let scroll_container =
            ScrollContainer(container)
                .Position(Position.Shrink(PAGE_MARGIN_X, PAGE_MARGIN_Y).SliceL(PAGE_ITEM_WIDTH))

        let search_box =
            SearchBox(fun query ->
                if query = "" then
                    container.Filter <- K true
                else
                    let query = query.Trim()
                    container.Filter <-
                    function
                    | :? PageSetting as p -> p.Label.Contains(query, System.StringComparison.InvariantCultureIgnoreCase)
                    | _ -> false
            )
                .Fill(Colors.cyan.O3)
                .Border(Colors.cyan_accent)
                .TextColor(Colors.text_cyan)
                .KeyboardAutoSelect()
                .Position(Position.SliceT(40.0f, SearchBox.HEIGHT).Shrink(PAGE_MARGIN_X, 0.0f).SliceR(500.0f))

        let hotkey_editor (hotkey: Hotkey) =
            NavigationContainer.Row()
                .With(
                    Keybinder(hotkey)
                        .Position(Position.ShrinkR(PAGE_ITEM_HEIGHT)),
                    Button(Icons.REFRESH_CCW, (fun () -> Hotkeys.reset hotkey))
                        .Position(Position.SliceR(PAGE_ITEM_HEIGHT))
                )

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
            .With(scroll_container, search_box)

    override this.Title = %"system.hotkeys"
    override this.OnClose() = ()