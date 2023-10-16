namespace Interlude.Features.OptionsMenu.Noteskins

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Prelude.Common
open Prelude.Data.Content
open Interlude.Content
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Features.OptionsMenu.Gameplay

type TextureEditGridItem(sprite: Sprite, x: int, y: int, selected: bool array array) =
    inherit StaticContainer(NodeType.Button (fun () -> Style.click.Play(); selected.[x].[y] <- not selected.[x].[y]))

    override this.Init(parent) =
        this 
        |+ Frame(NodeType.None, 
            Fill = (fun () -> if selected.[x].[y] then Colors.pink_accent.O2 elif this.Focused then Colors.yellow_accent.O2 else Color.Transparent),
            Border = (fun () -> if this.Focused then Colors.white.O3 elif selected.[x].[y] then Colors.pink_accent else Color.Transparent)
            )
        |* Clickable(this.Select, OnHover = fun b -> if b then if Mouse.held Mouse.LEFT then this.Select() else this.Focus())
        base.Init parent

    override this.OnFocus() = Style.hover.Play(); base.OnFocus()

    override this.Draw() =
        base.Draw()
        Draw.quad (this.Bounds |> Quad.ofRect) (Quad.colorOf Color.White) (Sprite.gridUV (x, y) sprite)

type DeleteButton(onClick) =
    inherit Button(K Icons.delete, onClick, Floating = true)

    member val VerticalPad = 0.0f with get, set

    override this.Draw() =
        if this.Focused then Draw.rect (this.Bounds.Expand(0.0f, this.VerticalPad)) Colors.yellow_accent.O2
        base.Draw()

type TextureEditGrid(sprite: Sprite, max_frames: int, max_colors: int) as this =
    inherit StaticContainer(NodeType.Switch (fun () -> this.Items))

    let mutable selected : bool array array = [||]
    let mutable items : NavigationContainer.Grid<Widget> = Unchecked.defaultof<_>

    member this.Refresh() =
        if sprite.Columns <> selected.Length || sprite.Rows <> selected.[0].Length then
            selected <- Array.init sprite.Columns (fun _ -> Array.zeroCreate sprite.Rows)
        let size = 
            min 
                ((this.Bounds.Width - 10.0f * float32 (sprite.Columns - 1)) / float32 sprite.Columns) 
                ((this.Bounds.Height - 10.0f * float32 (sprite.Rows - 1)) / float32 sprite.Rows)
        let grid_width = size * float32 sprite.Columns + 10.0f * float32 (sprite.Columns - 1)
        items <- NavigationContainer.Grid(WrapNavigation = false, Floating = true, Position = Position.Box(0.5f, 0.0f, -grid_width * 0.5f, 0.0f, grid_width, this.Bounds.Height))
        
        let grid = NavigationContainer.Grid<Widget>(WrapNavigation = false, Floating = true)

        for r = 0 to sprite.Rows - 1 do
            for c = 0 to sprite.Columns - 1 do

                grid.Add(
                    TextureEditGridItem(sprite, c, r, selected, Position = Position.Box(0.0f, 0.0f, float32 c * (size + 10.0f), float32 r * (size + 10.0f), size, size)),
                    c + 2,
                    r + 2)

                if r = 0 then
                    grid.Add(Text(K (sprintf "Frame %i" (c + 1)),
                        Color = K Colors.text_subheading,
                        Align = Alignment.CENTER,
                        Position = Position.Box(0.0f, 0.0f, float32 c * (size + 10.0f), -90.0f, size, 40.0f)), c + 2, 0)

                    grid.Add(DeleteButton((fun () -> printfn "delete frame %i" c),
                        Position = Position.Box(0.0f, 0.0f, float32 c * (size + 10.0f), -50.0f, size, 40.0f)), c + 2, 1)

            grid.Add(Text(K (sprintf "Color %i" (r + 1)),
                Color = K Colors.text_subheading,
                Align = Alignment.RIGHT,
                Position = Position.Box(0.0f, 0.0f, -250.0f, float32 r * (size + 10.0f), 200.0f, size).Margin(10.0f, size * 0.5f - 20.0f)), 0, r + 1)

            grid.Add(DeleteButton((fun () -> printfn "delete color %i" r),
                VerticalPad = size * 0.5f - 20.0f,
                Position = Position.Box(0.0f, 0.0f, -50.0f, float32 r * (size + 10.0f), 40.0f, size).Margin(0.0f, size * 0.5f - 20.0f)), 1, r + 2)

        items.Add(grid, 0, 0)

        if sprite.Rows < max_colors then
            items.Add(
                { new Button(K (sprintf "%s %s" Icons.add "Add color"), (fun () -> printfn "add color"), 
                    Floating = true,
                    Position = Position.Margin(0.0f, -50.0f).SliceBottom(40.0f)) with 
                    override this.Draw() = 
                        if this.Focused then Draw.rect this.Bounds Colors.yellow_accent.O2
                        base.Draw() 
                }, 0, 1)

        if sprite.Columns < max_frames then
            items.Add(
                { new Button(K Icons.add, (fun () -> printfn "add frame"), 
                    Floating = true,
                    Position = Position.Margin(-50.0f, 0.0f).SliceRight(40.0f)) with 
                    override this.Draw() = 
                        if this.Focused then Draw.rect this.Bounds Colors.yellow_accent.O2
                        base.Draw() 
                }, 1, 0)
        items.Init this

    member this.SelectedTextures =
        seq {
            for c = 0 to selected.Length - 1 do
                for r = 0 to selected.[c].Length - 1 do
                    if selected.[c].[r] then yield (c, r)
        }

    member private this.Items = items

    override this.Init(parent) =
        base.Init parent
        this.Refresh()

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        items.Update(elapsedTime, moved)

    override this.Draw() =
        base.Draw()
        items.Draw()

type TextureEditPage(texture_id) as this =
    inherit Page()

    let sprite = getTexture texture_id
    let stitched = Setting.simple false

    do
        this.Content(
            column()
            |+ TextureEditGrid(sprite, 16, 16, Position = Position.Box(0.5f, 0.0f, -375.0f, 200.0f, 750.0f, 750.0f))
        )
        
    override this.Title = "Texture: " + texture_id
    override this.OnClose() = ()

type TextureCard(id: string, on_click: unit -> unit) as this =
    inherit Frame(NodeType.Button (fun () -> Style.click.Play(); on_click()),
        Fill = (fun () -> if this.Focused then Colors.yellow_accent.O1 else Colors.shadow_2.O2),
        Border = (fun () -> if this.Focused then Colors.yellow_accent else Colors.grey_2.O3))

    let sprite = getTexture id

    do
        this
        |+ Image(sprite, Position = Position.Margin(20.0f))
        |+ Text(id,
            Align = Alignment.CENTER,
            Position = Position.Margin(Style.PADDING).SliceBottom(25.0f))
        |* Clickable.Focus this

    override this.OnFocus() = Style.hover.Play(); base.OnFocus()

type EditNoteskinPage(from_hotkey: bool) as this =
    inherit Page()

    let data = Noteskins.Current.config
        
    let name = Setting.simple data.Name

    let preview = NoteskinPreview 0.35f

    do
        this.Content(
            column()
            |+ PageTextEntry("noteskins.edit.noteskinname", name)
                .Pos(200.0f)

            |+ PageButton("noteskins.edit.playfield", fun () -> { new PlayfieldSettingsPage() with override this.OnClose() = preview.Refresh(); base.OnClose() }.Show())
                .Pos(300.0f)
                .Tooltip(Tooltip.Info("noteskins.edit.playfield"))
            |+ PageButton("noteskins.edit.holdnotes", fun () -> { new HoldNoteSettingsPage() with override this.OnClose() = preview.Refresh(); base.OnClose() }.Show())
                .Pos(370.0f)
                .Tooltip(Tooltip.Info("noteskins.edit.holdnotes"))
            |+ PageButton("noteskins.edit.colors", fun () -> { new ColorSettingsPage() with override this.OnClose() = preview.Refresh(); base.OnClose() }.Show())
                .Pos(440.0f)
                .Tooltip(Tooltip.Info("noteskins.edit.colors"))
            |+ PageButton("noteskins.edit.rotations", fun () -> { new RotationSettingsPage() with override this.OnClose() = preview.Refresh(); base.OnClose() }.Show())
                .Pos(510.0f)
                .Tooltip(Tooltip.Info("noteskins.edit.rotations"))
            |+ PageButton("noteskins.edit.animations", fun () -> { new AnimationSettingsPage() with override this.OnClose() = preview.Refresh(); base.OnClose() }.Show())
                .Pos(580.0f)
                .Tooltip(Tooltip.Info("noteskins.edit.animations"))
            |+ (
                GridFlowContainer<TextureCard>(150.0f, 4, 
                    WrapNavigation = false,
                    Spacing = (15.0f, 15.0f),
                    Position = Position.Box(0.0f, 0.0f, 100.0f, 680.0f, 645.0f, 315.0f))
                |+ TextureCard("note", (fun () -> TextureEditPage("note").Show()))
                |+ TextureCard("holdhead", (fun () -> TextureEditPage("holdhead").Show()))
                |+ TextureCard("holdbody", (fun () -> TextureEditPage("holdbody").Show()))
                |+ TextureCard("holdtail", (fun () -> TextureEditPage("holdtail").Show()))
                |+ TextureCard("receptor", (fun () -> TextureEditPage("receptor").Show()))
                |+ TextureCard("noteexplosion", (fun () -> TextureEditPage("noteexplosion").Show()))
                |+ TextureCard("holdexplosion", (fun () -> TextureEditPage("holdexplosion").Show()))
                |+ TextureCard("receptorlighting", (fun () -> TextureEditPage("receptorlighting").Show())) // todo: this one is different
                )
            |+ preview
        )
        this.Add (Conditional((fun () -> not from_hotkey),
            Callout.frame (Callout.Small.Icon(Icons.info).Title(L"noteskins.edit.hotkey_hint").Hotkey("edit_noteskin")) 
                ( fun (w, h) -> Position.SliceTop(h + 40.0f + 40.0f).SliceRight(w + 40.0f).Margin(20.0f, 20.0f) )
            ))
        
    override this.Title = data.Name
    override this.OnClose() =
        Noteskins.Current.changeConfig
            { Noteskins.Current.config with
                Name = name.Value
            }