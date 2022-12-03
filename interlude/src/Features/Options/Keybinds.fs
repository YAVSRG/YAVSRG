namespace Interlude.Features.OptionsMenu

open OpenTK.Windowing.GraphicsLibraryFramework
open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude.Common
open Interlude.Options
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Menu

module Keybinds = 

    type GameplayKeybinder(keymode: Setting<Keymode>) as this =
        inherit StaticContainer(NodeType.Leaf)

        let mutable progress = 0

        let mutable text = options.GameplayBinds.[int keymode.Value - 3] |> Seq.map (sprintf "%O") |> String.concat ",  "
        let refreshText() : unit =
            let binds = options.GameplayBinds.[int keymode.Value - 3]
            if not this.Selected then
                text <- binds |> Seq.map (sprintf "%O") |> String.concat ",  "
            else
                text <- ""
                for i = 0 to progress - 1 do
                    text <- text + binds.[i].ToString() + ",  "
                text <- text + "..."

        let rec inputCallback(b) =
            let binds = options.GameplayBinds.[int keymode.Value - 3]
            match b with
            | Key (k, _) ->
                binds.[progress] <- Key (k, (false, false, false))
                progress <- progress + 1
                if progress = int keymode.Value then this.Focus()
                else Input.grabNextEvent inputCallback
                refreshText()
            | _ -> Input.grabNextEvent inputCallback

        do
            this
            |+ Text((fun () -> text),
                Color = (fun () -> (if this.Selected then Style.color(255, 1.0f, 0.5f) else Color.White), Color.Black),
                Align = Alignment.LEFT)
            |* Clickable((fun () -> if not this.Selected then this.Select()),
                OnHover = fun b -> if b then this.Focus())

        override this.OnSelected() =
            base.OnSelected()
            progress <- 0
            refreshText()
            Input.grabNextEvent inputCallback

        override this.OnDeselected() =
            base.OnDeselected()
            Input.removeInputMethod()
            text <- options.GameplayBinds.[int keymode.Value - 3] |> Seq.map (sprintf "%O") |> String.concat ",  "

        member this.OnKeymodeChanged() = refreshText()

    type Keybinder(hotkey: Hotkey) as this =
        inherit StaticContainer(NodeType.Leaf)

        let mutable blocked = true

        do
            this
            |+ Text((fun () -> (!|hotkey).ToString()),
                Color = (fun () -> (if this.Selected then Style.color(255, 1.0f, 0.0f) else Color.White), Color.Black),
                Align = Alignment.LEFT,
                Position = Position.TrimLeft 20.0f)
            |* Clickable((fun () -> if not this.Selected then this.Select()), 
                OnHover = fun b -> if b then this.Focus())

        let set = fun v -> Hotkeys.set hotkey v
    
        override this.Draw() =
            if this.Selected then Draw.rect this.Bounds (!*Palette.SELECTED)
            elif this.Focused then Draw.rect this.Bounds (!*Palette.HOVER)
            base.Draw()

        override this.OnSelected() = base.OnSelected(); blocked <- true
    
        override this.Update(elapsedTime, bounds) =
            base.Update(elapsedTime, bounds)
            if this.Selected && not blocked then
                match Input.consumeAny InputEvType.Release with
                | ValueNone -> ()
                | ValueSome b ->
                    match b with
                    | Key (k, (ctrl, _, shift)) ->
                        if k = Keys.Escape then set Bind.Dummy
                        else set (Key (k, (ctrl, false, shift)))
                        this.Focus()
                    | _ -> ()
            if this.Selected && blocked && not ((!|"select").Pressed()) then blocked <- false

    type HotkeysPage() as this =
        inherit Page()

        do
            let hotkeyEditor hk =
                SwitchContainer.Row<Widget>()
                |+ Keybinder(hk, Position = Position.TrimRight PRETTYHEIGHT)
                |+ Button(Icons.reset, (fun () -> Hotkeys.reset hk), Position = Position.SliceRight PRETTYHEIGHT)

            let container = FlowContainer.Vertical<Widget>(PRETTYHEIGHT)
            let scrollContainer = ScrollContainer.Flow(container, Position = Position.Margin(100.0f, 200.0f))

            container.Add(PrettyButton(
                "keybinds.hotkeys.reset",
                (fun () -> Menu.ShowPage (ConfirmPage (L"options.keybinds.hotkeys.reset.confirm", Hotkeys.reset_all))),
                Icon = Icons.reset))

            for hk in Hotkeys.hotkeys.Keys do
                if hk <> "none" then
                    container.Add( PrettySetting("hotkeys." + hk.ToLower(), hotkeyEditor hk) )
            this.Content scrollContainer

        override this.Title = N"keybinds.hotkeys"
        override this.OnClose() = ()

    type KeybindsPage() as this =
        inherit Page()
        
        let keycount = Setting.simple options.KeymodePreference.Value
        let binds = GameplayKeybinder(keycount)

        do
            this.Content(
                column()
                |+ PrettySetting("generic.keymode", Selector<_>.FromEnum(keycount |> Setting.trigger (ignore >> binds.OnKeymodeChanged))).Pos(200.0f)
                |+ PrettySetting("keybinds.gameplay", binds).Pos(280.0f, Viewport.vwidth - 200.0f)
                |+ PrettyButton("keybinds.hotkeys", (fun () -> Menu.ShowPage HotkeysPage)).Pos(400.0f)
            )
        override this.Title = N"keybinds"
        override this.OnClose() = ()