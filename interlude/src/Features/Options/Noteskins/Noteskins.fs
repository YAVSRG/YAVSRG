namespace Interlude.Features.OptionsMenu.Noteskins

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.Data.Content
open Interlude.Content
open Interlude.Options
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Features.OptionsMenu.Gameplay

module private PreviewCleanup =

    let mutable private list = Set.empty

    let add (s: Sprite) = list <- list.Add s
    let clear () = Set.iter Sprite.destroy list; list <- Set.empty

type private NoteskinButton(id: string, ns: Noteskin, on_switch: unit -> unit) =
    inherit StaticContainer(NodeType.Button (fun _ -> if Noteskins.Current.id <> id then Noteskins.Current.switch id; options.Noteskin.Set id; Style.click.Play(); on_switch()))

    let mutable preview : Sprite option = None
    let imgFade = Animation.Fade 0.0f

    member this.IsCurrent = Noteskins.Current.id = id
        
    override this.Init(parent: Widget) =
        Noteskins.preview_loader.Request(
            ns,
            function
            | Some (bmp, config) ->
                sync(fun () ->
                    preview <- Some (Sprite.upload (bmp, config.Rows, config.Columns, true))
                    PreviewCleanup.add preview.Value
                    bmp.Dispose()
                    imgFade.Target <- 1.0f
                )
            | None -> ()
        )
        this
        |+ Text(
            K ns.Config.Name,
            Color = ( 
                fun () -> 
                    if this.Focused then Colors.text_yellow_2 
                    elif this.IsCurrent then Colors.text_pink
                    else Colors.text
            ),
            Align = Alignment.LEFT,
            Position = Position.TrimLeft(100.0f).Margin(Style.PADDING).SliceTop(70.0f))
        |+ Text(
            K (sprintf "Created by %s" ns.Config.Author),
            Color = K Colors.text_subheading,
            Align = Alignment.LEFT,
            Position = Position.TrimLeft(100.0f).Margin(7.5f, Style.PADDING).SliceBottom(30.0f))
        |* Clickable.Focus this
        base.Init parent

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        imgFade.Update elapsedTime
        
    override this.OnFocus() = Style.hover.Play(); base.OnFocus()
        
    override this.Draw() =
        match preview with
        | Some p -> 
            Draw.sprite (this.Bounds.SliceLeft 100.0f) (Colors.white.O4a imgFade.Alpha) p
        | None -> ()
        if this.IsCurrent then Draw.rect this.Bounds Colors.pink_accent.O1
        elif this.Focused then Draw.rect this.Bounds Colors.yellow_accent.O1
        base.Draw()

type NoteskinsPage() as this =
    inherit Page()

    let preview = NoteskinPreview 0.35f
    let grid = GridContainer<NoteskinButton>(100.0f, 2, WrapNavigation = false, Spacing = (20.0f, 20.0f))

    let rec tryEditNoteskin() =
        let ns = Noteskins.Current.instance
        match ns.Source with
        | Zip (_, Some file) ->
            ConfirmPage(
                Localisation.localiseWith [ns.Config.Name] "noteskins.confirm_extract_zip",
                (fun () -> 
                    if Noteskins.extractCurrent() then refresh()
                    else Logging.Error "Noteskin folder already exists"
                )).Show()
        | Zip (_, None) ->
            ConfirmPage(
                Localisation.localiseWith [ns.Config.Name] "noteskins.confirm_extract_default", 
                (fun () -> 
                    if Noteskins.extractCurrent() then refresh()
                    else Logging.Error "Noteskin folder already exists"
                )).Show()
        | Folder _ -> Menu.ShowPage { new EditNoteskinPage(false) with override this.OnClose() = base.OnClose(); refresh() }

    and refresh() =
        grid.Clear()
        preview.Refresh()

        for id, noteskin in Noteskins.list() do
            grid |* NoteskinButton(id, noteskin, preview.Refresh)

    do
        refresh()
        this.Content(
            SwitchContainer.Column<Widget>()
            |+
                (
                    FlowContainer.LeftToRight<Widget>(250.0f, Position = Position.Row(230.0f, 50.0f).Margin(100.0f, 0.0f))
                    |+ Button(Icons.edit + " " + L"noteskins.edit.name", tryEditNoteskin).Tooltip(Tooltip.Info("noteskins.edit"))
                    |+ Button(Icons.edit + " " + L"noteskins.edit.export.name", 
                        fun () -> if not (Noteskins.exportCurrent()) then Notifications.error(L"notification.export_noteskin_failure.title", L"notification.export_noteskin_failure.body"))
                        .Tooltip(Tooltip.Info("noteskins.edit.export"))
                )
            |+ Text("Current", Position = Position.Row(100.0f, 50.0f).Margin(100.0f, 0.0f), Color = K Colors.text_subheading, Align = Alignment.LEFT)
            |+ Text((fun () -> Noteskins.Current.config.Name), Position = Position.Row(130.0f, 100.0f).Margin(100.0f, 0.0f), Color = K Colors.text, Align = Alignment.LEFT)
            |+ ScrollContainer.Grid(grid, Position = { Left = 0.0f %+ 100.0f; Right = 0.6f %- 0.0f; Top = 0.0f %+ 320.0f; Bottom = 1.0f %- 270.0f })
            |+ PageButton("noteskins.open_folder", fun () -> openDirectory (getDataPath "Noteskins"))
                .Pos(830.0f)
                .Tooltip(Tooltip.Info("noteskins.open_folder"))
            |+ PageButton("noteskins.get_more", 
                (fun () -> 
                    Menu.Exit()
                    Interlude.Features.Import.ImportScreen.switch_to_noteskins()
                    Screen.change Screen.Type.Import Transitions.Flags.Default
                ))
                .Pos(900.0f)
        )
        this |* preview

    override this.Title = L"noteskins.name"
    override this.OnDestroy() = 
        preview.Destroy()
        PreviewCleanup.clear()
    override this.OnClose() = ()