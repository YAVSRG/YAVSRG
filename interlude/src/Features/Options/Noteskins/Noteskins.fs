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
                    bmp.Dispose()
                    imgFade.Target <- 1.0f
                )
            | None -> ()
        )
        this
        |+ Text(
            (fun () -> sprintf "%s  %s" ns.Config.Name (if options.SelectedRuleset.Value = id then Icons.selected else "")),
            Color = ( 
                fun () -> 
                    if this.Focused then Colors.text_yellow_2 
                    elif this.IsCurrent then Colors.text_pink
                    else Colors.text
            ),
            Align = Alignment.CENTER,
            Position = Position.TrimLeft(PRETTYHEIGHT).Margin Style.PADDING)
        |* Clickable.Focus this
        base.Init parent

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        imgFade.Update elapsedTime
        
    override this.OnFocus() = Style.hover.Play(); base.OnFocus()
        
    override this.Draw() =
        match preview with
        | Some p -> 
            Draw.sprite (this.Bounds.SliceLeft PRETTYHEIGHT) (Colors.white.O4a imgFade.Alpha) p
        | None -> ()
        if this.IsCurrent then Draw.rect this.Bounds Colors.pink_accent.O1
        elif this.Focused then Draw.rect this.Bounds Colors.yellow_accent.O1
        base.Draw()

type NoteskinsPage() as this =
    inherit Page()

    let preview = NoteskinPreview 0.35f
    let grid = GridContainer<NoteskinButton>(PRETTYHEIGHT, 2, WrapNavigation = false, Spacing = (20.0f, 20.0f))

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
        | Folder _ -> Menu.ShowPage { new EditNoteskinPage() with override this.OnClose() = base.OnClose(); refresh() }

    and refresh() =
        grid.Clear()

        for id, noteskin in Noteskins.list() do
            grid |* NoteskinButton(id, noteskin, preview.Refresh)

    do
        refresh()
        this.Content(
            SwitchContainer.Column<Widget>()
            |+ PageButton("noteskins.get_more", 
                (fun () -> 
                    Menu.Exit()
                    Interlude.Features.Import.ImportScreen.switch_to_noteskins()
                    Screen.change Screen.Type.Import Transitions.Flags.Default
                ))
                .Pos(200.0f)
                .Tooltip(Tooltip.Info("noteskins.edit"))
            |+ PageButton("noteskins.edit", tryEditNoteskin)
                .Pos(300.0f)
                .Tooltip(Tooltip.Info("noteskins.edit"))
            |+ PageButton("noteskins.open_folder", fun () -> openDirectory (getDataPath "Noteskins"))
                .Pos(370.0f)
                .Tooltip(Tooltip.Info("noteskins.open_folder"))
            |+ ScrollContainer.Grid(grid, Position = { Left = 0.0f %+ 100.0f; Right = 0.6f %- 0.0f; Top = 0.0f %+ 470.0f; Bottom = 1.0f %- 150.0f })
        )
        this |* preview

    override this.Title = L"noteskins.name"
    override this.OnDestroy() = 
        preview.Destroy()
        PreviewCleanup.clear()
    override this.OnClose() = ()