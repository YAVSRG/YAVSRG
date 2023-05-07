namespace Interlude.Features.OptionsMenu.Gameplay

open Prelude.Gameplay.NoteColors
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.Data.Content
open Interlude.Content
open Interlude.Options
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Menu
open Interlude.UI.Components

type NoteColorPicker(color: Setting<byte>) as this =
    inherit StaticContainer(NodeType.Leaf)
    
    let sprite = getTexture "note"
    let n = byte sprite.Rows
    
    let fd() = Setting.app (fun x -> (x + n - 1uy) % n) color
    let bk() = Setting.app (fun x -> (x + 1uy) % n) color
    
    do 
        this
        |* Clickable((fun () -> (if not this.Selected then this.Select()); fd ()), OnHover = fun b -> if b && not this.Focused then this.Focus())
    
    override this.Draw() =
        base.Draw()
        if this.Selected then Draw.rect this.Bounds Colors.pink_accent.O2
        elif this.Focused then Draw.rect this.Bounds Colors.yellow_accent.O2
        Draw.quad (Quad.ofRect this.Bounds) (Quad.colorOf Color.White) (Sprite.gridUV (3, int color.Value) sprite)
    
    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
            
        if this.Selected then
            if (!|"up").Tapped() then fd()
            elif (!|"down").Tapped() then bk()
            elif (!|"left").Tapped() then bk()
            elif (!|"right").Tapped() then fd()

type EditNoteskinPage() as this =
    inherit Page()

    let data = Noteskins.Current.config
        
    let name = Setting.simple data.Name
    let holdNoteTrim = Setting.bounded data.HoldNoteTrim 0.0f 2.0f |> Setting.roundf 2
    let columnWidth = Setting.bounded data.ColumnWidth 10.0f 300.0f |> Setting.roundf 0
    let columnSpacing = Setting.bounded data.ColumnSpacing 0.0f 100.0f |> Setting.roundf 0
    let enableColumnLight = Setting.simple data.EnableColumnLight
    let keycount = Setting.simple options.KeymodePreference.Value
    let mutable noteColors = data.NoteColors
        
    let g keycount i =
        let k = if noteColors.UseGlobalColors then 0 else int keycount - 2
        Setting.make
            (fun v -> noteColors.Colors.[k].[i] <- v)
            (fun () -> noteColors.Colors.[k].[i])

    let colors, refreshColors =
        refreshRow
            (fun () -> ColorScheme.count (int keycount.Value) noteColors.Style)
            (fun i k ->
                let x = -60.0f * float32 k
                let n = float32 i
                NoteColorPicker(g keycount.Value i, Position = { Position.Default with Left = 0.5f %+ (x + 120.0f * n); Right = 0.5f %+ (x + 120.0f * n + 120.0f) })
            )

    do
        this.Content(
            column()
            |+ PageSetting("gameplay.noteskins.edit.noteskinname", TextEntry(name, "none"))
                .Pos(100.0f)
            |+ PageSetting("gameplay.noteskins.edit.holdnotetrim", Slider(holdNoteTrim))
                .Pos(170.0f)
                .Tooltip(Tooltip.Info("gameplay.noteskins.edit.holdnotetrim"))
            |+ PageSetting("gameplay.noteskins.edit.enablecolumnlight", Selector<_>.FromBool enableColumnLight)
                .Pos(240.0f)
                .Tooltip(Tooltip.Info("gameplay.noteskins.edit.enablecolumnlight"))
            |+ PageSetting("gameplay.noteskins.edit.columnwidth", Slider(columnWidth, Step = 1f))
                .Pos(310.0f)
                .Tooltip(Tooltip.Info("gameplay.noteskins.edit.columnwidth"))
            |+ PageSetting("gameplay.noteskins.edit.columnspacing",Slider(columnSpacing, Step = 1f))
                .Pos(390.0f)
                .Tooltip(Tooltip.Info("gameplay.noteskins.edit.columnspacing"))
            |+ PageSetting("generic.keymode",
                    Selector<Keymode>.FromEnum(keycount |> Setting.trigger (ignore >> refreshColors)) )
                .Pos(490.0f)
            |+ PageSetting("gameplay.noteskins.edit.globalcolors",
                    Selector<_>.FromBool(
                        Setting.make
                            (fun v -> noteColors <- { noteColors with UseGlobalColors = v })
                            (fun () -> noteColors.UseGlobalColors)
                        |> Setting.trigger (ignore >> refreshColors)) )
                .Pos(560.0f)
                .Tooltip(Tooltip.Info("gameplay.noteskins.edit.globalcolors"))
            |+ PageSetting("gameplay.noteskins.edit.colorstyle",
                    Selector.FromEnum(
                        Setting.make
                            (fun v -> noteColors <- { noteColors with Style = v })
                            (fun () -> noteColors.Style)
                        |> Setting.trigger (ignore >> refreshColors)) )
                .Pos(630.0f)
                .Tooltip(Tooltip.Info("gameplay.noteskins.edit.colorstyle"))
            |+ PageSetting("gameplay.noteskins.edit.notecolors", colors)
                .Pos(700.0f, Viewport.vwidth - 200.0f, 120.0f)
                .Tooltip(Tooltip.Info("gameplay.noteskins.edit.notecolors"))
            |+ PageButton.Once("gameplay.noteskins.edit.export", fun () -> if not (Noteskins.exportCurrent()) then Notifications.error(L"notification.export_noteskin_failure.title", L"notification.export_noteskin_failure.body"))
                .Pos(820.0f)
                .Tooltip(Tooltip.Info("gameplay.noteskins.edit.export"))
        )

    override this.Title = data.Name
    override this.OnClose() =
        Noteskins.Current.changeConfig
            { data with
                Name = name.Value
                HoldNoteTrim = holdNoteTrim.Value
                EnableColumnLight = enableColumnLight.Value
                NoteColors = noteColors
                ColumnWidth = columnWidth.Value
                ColumnSpacing = columnSpacing.Value
            }

type private NoteskinButton(id: string, name: string, on_switch: unit -> unit) =
    inherit StaticContainer(NodeType.Button (fun _ -> Noteskins.Current.switch id; on_switch()))

    member this.IsCurrent = Noteskins.Current.id = id
        
    override this.Init(parent: Widget) =
        this
        |+ Text(
            (fun () -> sprintf "%s  %s" name (if options.SelectedRuleset.Value = id then Icons.selected else "")),
            Color = ( 
                fun () -> 
                    if this.Focused then Colors.text_yellow_2 
                    elif this.IsCurrent then Colors.text_pink
                    else Colors.text
            ),
            Align = Alignment.LEFT,
            Position = Position.SliceTop(PRETTYHEIGHT).Margin Style.padding)
        |* Clickable.Focus this
        base.Init parent
        
    override this.Draw() =
        if this.IsCurrent then Draw.rect this.Bounds Colors.pink_accent.O1
        elif this.Focused then Draw.rect this.Bounds Colors.yellow_accent.O1
        base.Draw()

type NoteskinsPage() as this =
    inherit Page()

    let preview = NoteskinPreview 0.5f

    do
        let container = FlowContainer.Vertical<Widget>(PRETTYHEIGHT)

        let tryEditNoteskin() =
            let ns = Noteskins.Current.instance
            match ns.Source with
            | Zip (_, Some file) ->
                ConfirmPage(
                    Localisation.localiseWith [ns.Config.Name] "gameplay.noteskins.confirm_extract_zip",
                    (fun () -> 
                        if Noteskins.extractCurrent() then ()
                        else Logging.Error "Noteskin folder already exists"
                    )).Show()
            | Zip (_, None) ->
                ConfirmPage(
                    Localisation.localiseWith [ns.Config.Name] "gameplay.noteskins.confirm_extract_default", 
                    (fun () -> 
                        if Noteskins.extractCurrent() then ()
                        else Logging.Error "Noteskin folder already exists"
                    )).Show()
            | Folder _ -> Menu.ShowPage EditNoteskinPage

        container
        |+ PageButton("gameplay.noteskins.edit", tryEditNoteskin)
            .Pos(570.0f)
            .Tooltip(Tooltip.Info("gameplay.noteskins.edit"))
        |+ PageButton("gameplay.noteskins.open_folder", fun () -> openDirectory (getDataPath "Noteskins"))
            .Pos(640.0f)
            .Tooltip(Tooltip.Info("gameplay.noteskins.open_folder"))
        |* Dummy()

        for id, name in Noteskins.list() do
            container |* NoteskinButton(id, name, preview.Refresh)
        this.Content( ScrollContainer.Flow(container, Position = Position.Margin(100.0f, 150.0f)) )
        this |* preview

    override this.Title = L"gameplay.noteskins.name"
    override this.OnDestroy() = preview.Destroy()
    override this.OnClose() = ()