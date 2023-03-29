namespace Interlude.Features.OptionsMenu

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.Scoring
open Interlude.Content
open Interlude.Options
open Interlude.UI.Menu
open Interlude.UI
open Interlude.Utils
open Interlude.Features

module Gameplay = 

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

    type LanecoverPage() as this =
        inherit Page()

        let preview = Themes.NoteskinPreview 0.35f

        do
            this.Content(
                column()
                |+ PrettySetting("gameplay.lanecover.enabled", Selector<_>.FromBool options.LaneCover.Enabled)
                    .Pos(200.0f)
                |+ PrettySetting("gameplay.lanecover.hidden", Slider<_>.Percent(options.LaneCover.Hidden, 0.01f))
                    .Pos(300.0f)
                    .Tooltip(Tooltip.Info("options.gameplay.lanecover.hidden"))
                |+ PrettySetting("gameplay.lanecover.sudden", Slider<_>.Percent(options.LaneCover.Sudden, 0.01f))
                    .Pos(370.0f)
                    .Tooltip(Tooltip.Info("options.gameplay.lanecover.sudden"))
                |+ PrettySetting("gameplay.lanecover.fadelength", Slider(options.LaneCover.FadeLength, 0.01f))
                    .Pos(440.0f)
                    .Tooltip(Tooltip.Info("options.gameplay.lanecover.fadelength"))
                |+ PrettySetting("gameplay.lanecover.color", ColorPicker(options.LaneCover.Color, true))
                    .Pos(510.0f, PRETTYWIDTH, PRETTYHEIGHT * 2.0f)
                |+ preview
            )
        override this.Title = L"options.gameplay.lanecover.name"
        override this.OnDestroy() = preview.Destroy()
        override this.OnClose() = ()

    type GameplayPage() as this =
        inherit Page()

        let keycount = Setting.simple options.KeymodePreference.Value
        let binds = GameplayKeybinder(keycount)

        do
            this.Content(
                column()
                |+ PrettySetting("gameplay.scrollspeed", Slider<_>.Percent(options.ScrollSpeed, 0.0025f))
                    .Pos(200.0f)
                    .Tooltip(Tooltip.Info("options.gameplay.scrollspeed"))
                |+ PrettySetting("gameplay.hitposition", Slider(options.HitPosition, 0.005f))
                    .Pos(270.0f)
                    .Tooltip(Tooltip.Info("options.gameplay.hitposition"))
                |+ PrettySetting("gameplay.upscroll", Selector<_>.FromBool options.Upscroll)
                    .Pos(340.0f)
                    .Tooltip(Tooltip.Info("options.gameplay.upscroll"))
                |+ PrettySetting("gameplay.backgrounddim", Slider<_>.Percent(options.BackgroundDim, 0.01f))
                    .Pos(410.0f)
                    .Tooltip(Tooltip.Info("options.gameplay.backgrounddim"))

                |+ PrettySetting("generic.keymode", Selector<_>.FromEnum(keycount |> Setting.trigger (ignore >> binds.OnKeymodeChanged)))
                    .Pos(510.0f)
                |+ PrettySetting("gameplay.keybinds", binds)
                    .Pos(580.0f, Viewport.vwidth - 200.0f)
                    .Tooltip(Tooltip.Info("options.gameplay.keybinds"))

                |+ PrettyButton("gameplay.lanecover", fun() -> Menu.ShowPage LanecoverPage)
                    .Pos(680.0f)
                    .Tooltip(Tooltip.Info("options.gameplay.lanecover"))
                |+ PrettyButton("gameplay.pacemaker", fun () ->  Menu.ShowPage PacemakerPage)
                    .Pos(750.0f)
                    .Tooltip(Tooltip.Info("options.gameplay.pacemaker").Body(L"options.gameplay.pacemaker.hint"))
                |+ PrettyButton("gameplay.rulesets", fun () -> Menu.ShowPage Rulesets.FavouritesPage)
                    .Pos(820.0f)
                    .Tooltip(Tooltip.Info("options.gameplay.rulesets"))
            )
        override this.Title = L"options.gameplay.name"
        override this.OnClose() = ()
