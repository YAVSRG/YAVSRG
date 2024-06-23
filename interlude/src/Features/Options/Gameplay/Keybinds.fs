namespace Interlude.Features.OptionsMenu.Gameplay

open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Interlude.Options
open Interlude.UI.Menu
open Interlude.Features.Gameplay

type GameplayKeybinder(keymode: Setting<Keymode>) as this =
    inherit Container(NodeType.FocusTrap)

    let mutable progress = 0

    let mutable text =
        options.GameplayBinds.[int keymode.Value - 3]
        |> Seq.map (sprintf "%O")
        |> String.concat ",  "

    let refresh_text () : unit =
        let binds = options.GameplayBinds.[int keymode.Value - 3]

        if not this.Selected then
            text <- binds |> Seq.map (sprintf "%O") |> String.concat ",  "
        else
            text <- ""

            for i = 0 to progress - 1 do
                text <- text + binds.[i].ToString() + ",  "

            text <- text + "..."

    let rec input_callback (b) =
        let binds = options.GameplayBinds.[int keymode.Value - 3]

        match b with
        | Key(k, _) ->
            // todo: prevent duplicates
            binds.[progress] <- Key(k, (false, false, false))
            progress <- progress + 1

            if progress = int keymode.Value then
                this.Focus false
            else
                Input.listen_to_next_key input_callback

            refresh_text ()
            Style.key.Play()
        | _ -> Input.listen_to_next_key input_callback

    do
        this
        |+ Text(
            (fun () -> text),
            Color = (fun () -> (if this.Selected then Colors.yellow_accent else Colors.white), Colors.shadow_1),
            Align = Alignment.LEFT
        )
        |* Clickable.Focus(
            this,
            OnHover =
                fun b ->
                    if b && not this.Focused then
                        this.Focus true
                    elif not b && this.FocusedByMouse && not this.Selected then
                        Selection.up true
        )

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
            options.GameplayBinds.[int keymode.Value - 3]
            |> Seq.map (sprintf "%O")
            |> String.concat ",  "

    member this.OnKeymodeChanged() = refresh_text ()

    static member KeymodeAndKeybinder() =
        let keymode: Setting<Keymode> = Setting.simple <| SelectedChart.keymode ()
        let binder = GameplayKeybinder(keymode, Position = Position.TrimLeft 100.0f)
        binder.Add { new StaticWidget(NodeType.None) with
            override this.Draw() = if binder.Focused then Draw.rect (this.Bounds.BorderBottom(Style.PADDING).SliceLeft(50.0f)) Colors.yellow_accent
        }
        let keymode_selector = Selector.FromEnum(keymode |> Setting.trigger (ignore >> binder.OnKeymodeChanged), Position = Position.SliceLeft 100.0f)
        keymode_selector.Add { new StaticWidget(NodeType.None) with
            override this.Draw() = if keymode_selector.Focused then Draw.rect (this.Bounds.BorderBottom(Style.PADDING).SliceLeft(50.0f)) Colors.yellow_accent
        }
        NavigationContainer.Row()
        |+ binder
        |+ keymode_selector