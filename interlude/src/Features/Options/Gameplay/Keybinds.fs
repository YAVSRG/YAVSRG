namespace Interlude.Features.OptionsMenu.Gameplay

open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude
open Interlude.Options
open Interlude.UI

type GameplayKeybinder(keymode: Keymode) as this =
    inherit Container(NodeType.FocusTrap)

    let mutable progress = 0

    let mutable text =
        options.GameplayBinds.[int keymode - 3]
        |> Seq.map (sprintf "%O")
        |> String.concat ",  "

    let refresh_text () : unit =
        let binds = options.GameplayBinds.[int keymode - 3]

        if not this.Selected then
            text <- binds |> Seq.map (sprintf "%O") |> String.concat ",  "
        else
            text <- ""

            for i = 0 to progress - 1 do
                text <- text + binds.[i].ToString() + ",  "

            text <- text + "..."

    let rec input_callback (b) =
        let binds = options.GameplayBinds.[int keymode - 3]

        match b with
        | Bind.Key(k, _) ->
            // todo: prevent duplicates
            binds.[progress] <- Bind.Key(k, (false, false, false))
            progress <- progress + 1

            if progress = int keymode then
                this.Focus false
            else
                Input.listen_to_next_key input_callback
                Input.finish_frame_events()

            refresh_text ()
            Style.key.Play()
        | _ ->
            Input.listen_to_next_key input_callback
            Input.finish_frame_events()

    do
        this
        |+ Text((fun () -> text))
            .Color((fun () -> (if this.Selected then Colors.yellow_accent else Colors.white), Colors.shadow_1))
            .Align(Alignment.LEFT)
        |* MouseListener().Button(this)

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.OnSelected(by_mouse: bool) =
        base.OnSelected by_mouse
        progress <- 0
        refresh_text ()
        Style.click.Play()
        Input.listen_to_next_key input_callback

    override this.OnDeselected(by_mouse: bool) =
        base.OnDeselected by_mouse
        Input.remove_listener ()

        text <-
            options.GameplayBinds.[int keymode - 3]
            |> Seq.map (sprintf "%O")
            |> String.concat ",  "

    member this.OnKeymodeChanged() = refresh_text ()

type GameplayBindsPage() =
    inherit Page()

    override this.Content() =
        page_container()
        |+ PageSetting("3K", GameplayKeybinder Keymode.``3K``).Pos(0, 2, PageWidth.Full)
        |+ PageSetting("4K", GameplayKeybinder Keymode.``4K``).Pos(2, 2, PageWidth.Full)
        |+ PageSetting("5K", GameplayKeybinder Keymode.``5K``).Pos(4, 2, PageWidth.Full)
        |+ PageSetting("6K", GameplayKeybinder Keymode.``6K``).Pos(6, 2, PageWidth.Full)
        |+ PageSetting("7K", GameplayKeybinder Keymode.``7K``).Pos(8, 2, PageWidth.Full)
        |+ PageSetting("8K", GameplayKeybinder Keymode.``8K``).Pos(10, 2, PageWidth.Full)
        |+ PageSetting("9K", GameplayKeybinder Keymode.``9K``).Pos(12, 2, PageWidth.Full)
        |+ PageSetting("10K", GameplayKeybinder Keymode.``10K``).Pos(14, 2, PageWidth.Full)
        :> Widget

    override this.Title = %"gameplay.keybinds"
    override this.OnClose() = ()