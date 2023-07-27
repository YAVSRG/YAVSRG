namespace Interlude.Features.OptionsMenu.Gameplay

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Audio
open Prelude.Common
open Interlude.Content
open Interlude.Options
open Interlude.UI.Menu
open Interlude.UI
open Interlude.UI.Components
open Interlude.Utils
open Interlude.Features

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
            Color = (fun () -> (if this.Selected then Colors.yellow_accent else Colors.white), Colors.shadow_1),
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

    let preview = NoteskinPreview 0.35f

    do
        this.Content(
            column()
            |+ PageSetting("gameplay.lanecover.enabled", Selector<_>.FromBool options.LaneCover.Enabled)
                .Pos(200.0f)
            |+ PageSetting("gameplay.lanecover.hidden", Slider.Percent(options.LaneCover.Hidden))
                .Pos(300.0f)
                .Tooltip(Tooltip.Info("gameplay.lanecover.hidden"))
            |+ PageSetting("gameplay.lanecover.sudden", Slider.Percent(options.LaneCover.Sudden))
                .Pos(370.0f)
                .Tooltip(Tooltip.Info("gameplay.lanecover.sudden"))
            |+ PageSetting("gameplay.lanecover.fadelength", Slider(options.LaneCover.FadeLength, Step = 5.0f))
                .Pos(440.0f)
                .Tooltip(Tooltip.Info("gameplay.lanecover.fadelength"))
            |+ PageSetting("gameplay.lanecover.color", ColorPicker(options.LaneCover.Color, true))
                .Pos(510.0f, PRETTYWIDTH, PRETTYHEIGHT * 2.0f)
            |+ preview
        )
    override this.Title = L"gameplay.lanecover.name"
    override this.OnDestroy() = preview.Destroy()
    override this.OnClose() = ()

type EditPresetPage(setting: Setting<Preset option>) as this =
    inherit Page()

    let mutable delete = false
    let deleteButton = PageButton("gameplay.preset.delete", fun () -> delete <- true; Menu.Back())

    let preset = setting.Value.Value
    let name = Setting.simple preset.Name
    let locked = Setting.simple preset.Locked |> Setting.trigger (fun b -> deleteButton.Enabled <- not b)

    do
        this.Content(
            column()
            |+ PageSetting("gameplay.preset.name", TextEntry(name, "none"))
                .Pos(200.0f)
            |+ PageSetting("gameplay.preset.locked", Selector<_>.FromBool(locked))
                .Pos(270.0f)
                .Tooltip(Tooltip.Info("gameplay.preset.locked"))
            |+ deleteButton
                .Pos(340.0f)
        )

    override this.Title = preset.Name
    override this.OnClose() = if delete then setting.Set None else setting.Set (Some { preset with Name = name.Value; Locked = locked.Value })


type GameplayPage() as this =
    inherit Page()

    let keycount = Setting.simple options.KeymodePreference.Value
    let binds = GameplayKeybinder(keycount)
    let preview = NoteskinPreview 0.35f

    let presetButtons (i: int) (setting: Setting<Preset option>) =
        StaticContainer(NodeType.None, Position = Position.Box(1.0f, 1.0f, -1200.0f + float32 i * 300.0f, -90.0f, 290.0f, 80.0f))
        |+ ButtonV2(
                (fun () ->
                    match setting.Value with
                    | None -> sprintf "Preset %i (Empty)" i
                    | Some s -> Icons.edit + " " + s.Name
                ),
                (fun () -> if setting.Value.IsSome then EditPresetPage(setting).Show()
                ),
                Disabled = (fun () -> setting.Value.IsNone),
                Position = Position.SliceTop(40.0f)
            )
        |+ ButtonV2(
                L"gameplay.preset.load",
                (fun () ->
                    match setting.Value with
                    | Some s -> 
                        ConfirmPage(Localisation.localiseWith [s.Name] "gameplay.preset.load.prompt", fun () ->
                            Presets.load s
                            preview.Refresh()
                            Notifications.action_feedback(Icons.system_notification, L"notification.preset_loaded", s.Name)
                        ).Show()
                    | None -> ()),
                Disabled = (fun () -> setting.Value.IsNone),
                Position = { Position.SliceBottom(40.0f) with Right = 0.5f %+ 0.0f }.Margin(40.0f, 0.0f)
            )
        |+ ButtonV2(
                L"gameplay.preset.save",
                (fun () ->
                    if setting.Value.IsNone then
                        let name = sprintf "Preset %i" i
                        setting.Value <- Presets.create(name) |> Some
                        Notifications.action_feedback(Icons.system_notification, L"notification.preset_saved", name)
                    else
                        let name = setting.Value.Value.Name
                        ConfirmPage(Localisation.localiseWith [name] "gameplay.preset.save.prompt", fun () ->
                            setting.Value <- Presets.create(name) |> Some
                            Notifications.action_feedback(Icons.system_notification, L"notification.preset_saved", name)
                        ).Show()
                ),
                Disabled = (fun () -> match setting.Value with Some s -> s.Locked | None -> false),
                Position = { Position.SliceBottom(40.0f) with Left = 0.5f %+ 0.0f }.Margin(40.0f, 0.0f)
            )

    do
        this.Content(
            column()
            |+ PageSetting("gameplay.scrollspeed", Slider.Percent(options.ScrollSpeed))
                .Pos(100.0f)
                .Tooltip(Tooltip.Info("gameplay.scrollspeed"))
            |+ PageSetting("gameplay.hitposition", Slider(options.HitPosition, Step = 1f))
                .Pos(170.0f)
                .Tooltip(Tooltip.Info("gameplay.hitposition"))
            |+ PageSetting("gameplay.upscroll", Selector<_>.FromBool options.Upscroll)
                .Pos(240.0f)
                .Tooltip(Tooltip.Info("gameplay.upscroll"))
            |+ PageSetting("gameplay.backgrounddim", Slider.Percent(options.BackgroundDim))
                .Pos(310.0f)
                .Tooltip(Tooltip.Info("gameplay.backgrounddim"))
            |+ PageSetting("system.audiooffset",
                    { new Slider(options.AudioOffset, Step = 1f)
                        with override this.OnDeselected() = base.OnDeselected(); Song.changeGlobalOffset (options.AudioOffset.Value * 1.0f<ms>) } )
                .Pos(380.0f)
                .Tooltip(Tooltip.Info("system.audiooffset"))
            |+ PageSetting("system.visualoffset", Slider(options.VisualOffset, Step = 1f))
                .Pos(450.0f)
                .Tooltip(Tooltip.Info("system.visualoffset"))

            |+ PageSetting("generic.keymode", Selector<_>.FromEnum(keycount |> Setting.trigger (ignore >> binds.OnKeymodeChanged)))
                .Pos(550.0f)
            |+ PageSetting("gameplay.keybinds", binds)
                .Pos(620.0f, Viewport.vwidth - 200.0f)
                .Tooltip(Tooltip.Info("gameplay.keybinds"))

            |+ PageButton("gameplay.lanecover", fun() -> Menu.ShowPage LanecoverPage)
                .Pos(720.0f)
                .Tooltip(Tooltip.Info("gameplay.lanecover"))
            |+ PageButton("gameplay.pacemaker", fun () ->  Menu.ShowPage PacemakerPage)
                .Pos(790.0f)
                .Tooltip(Tooltip.Info("gameplay.pacemaker").Body(L"gameplay.pacemaker.hint"))
            |+ PageButton("gameplay.rulesets", fun () -> Menu.ShowPage Rulesets.FavouritesPage)
                .Pos(860.0f)
                .Tooltip(Tooltip.Info("gameplay.rulesets"))
            |+ preview
            |+ presetButtons 1 options.Preset1
            |+ presetButtons 2 options.Preset2
            |+ presetButtons 3 options.Preset3
        )

    override this.Title = L"gameplay.name"
    override this.OnDestroy() = preview.Destroy()
    override this.OnClose() = ()
