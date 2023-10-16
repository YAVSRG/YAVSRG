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

type TextureEditGrid(texture_id: string, max_frames: int, max_colors: int) as this =
    inherit StaticContainer(NodeType.Switch (fun () -> this.Items))

    let mutable sprite = Sprite.Default
    let mutable selected : bool array array = [||]
    let mutable items : NavigationContainer.Grid<Widget> = Unchecked.defaultof<_>

    member this.Refresh() =
        sprite <- getTexture texture_id
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
                { new Button(K (sprintf "%s %s" Icons.add "Add color"), 
                    (fun () -> 
                        let src_row = match Seq.tryHead this.SelectedTextures with Some (x, y) -> y | None -> 0
                        ConfirmPage(
                            sprintf "Add a new color to this texture? (will be a copy of color %i)" (src_row + 1),
                            fun () -> 
                                Noteskins.Current.instance.AddTextureRow(src_row, texture_id)
                                Noteskins.Current.reload_texture(texture_id)
                                this.Refresh()
                            ).Show()
                    ), 
                    Floating = true,
                    Position = Position.Margin(0.0f, -50.0f).SliceBottom(40.0f)) with 
                    override this.Draw() = 
                        if this.Focused then Draw.rect this.Bounds Colors.yellow_accent.O2
                        base.Draw() 
                }, 0, 1)

        if sprite.Columns < max_frames then
            items.Add(
                { new Button(K Icons.add,
                    (fun () -> 
                        let src_col = match Seq.tryHead this.SelectedTextures with Some (x, y) -> x | None -> 0
                        ConfirmPage(
                            sprintf "Add a new animation frame to this texture? (will be a copy of frame %i)" (src_col + 1),
                            fun () -> 
                                Noteskins.Current.instance.AddTextureColumn(src_col, texture_id)
                                Noteskins.Current.reload_texture(texture_id)
                                this.Refresh()
                            ).Show()
                    ), 
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

type TextureEditPage(texture_id: string) as this =
    inherit Page()

    let max_frames, max_colors =
        match texture_id with
        | "receptor" -> 16, 2
        | _ -> 16, 16
    let texture_editor = TextureEditGrid(texture_id, max_frames, max_colors, Position = Position.Box(0.5f, 0.0f, -375.0f, 200.0f, 750.0f, 750.0f))

    do
        Noteskins.Current.instance.SplitTexture(texture_id)
        this.Content(
            column()
            |+ texture_editor
            |+ (
                FlowContainer.Vertical(45.0f, Spacing = 15.0f, Position = Position.SliceRight(400.0f).Margin(50.0f))
                |+ Button(
                    Icons.rotate_cw + " Rotate clockwise",
                    fun () -> 
                        for (col, row) in texture_editor.SelectedTextures do
                            Noteskins.Current.instance.RotateClockwise((col, row), texture_id)
                        Noteskins.Current.reload_texture(texture_id)
                        texture_editor.Refresh()
                    , Disabled = fun () -> texture_editor.SelectedTextures |> Seq.isEmpty
                    )
                |+ Button(
                    Icons.rotate_ccw + " Rotate anticlockwise",
                    fun () -> 
                        for (col, row) in texture_editor.SelectedTextures do
                            Noteskins.Current.instance.RotateAnticlockwise((col, row), texture_id)
                        Noteskins.Current.reload_texture(texture_id)
                        texture_editor.Refresh()
                    , Disabled = fun () -> texture_editor.SelectedTextures |> Seq.isEmpty
                    )
                |+ Button(
                    Icons.vertical_flip + " Vertical flip",
                    fun () -> 
                        for (col, row) in texture_editor.SelectedTextures do
                            Noteskins.Current.instance.VerticalFlipTexture((col, row), texture_id)
                        Noteskins.Current.reload_texture(texture_id)
                        texture_editor.Refresh()
                    , Disabled = fun () -> texture_editor.SelectedTextures |> Seq.isEmpty
                    )
                |+ Button(
                    Icons.horizontal_flip + " Horizontal flip",
                    fun () -> 
                        for (col, row) in texture_editor.SelectedTextures do
                            Noteskins.Current.instance.HorizontalFlipTexture((col, row), texture_id)
                        Noteskins.Current.reload_texture(texture_id)
                        texture_editor.Refresh()
                    , Disabled = fun () -> texture_editor.SelectedTextures |> Seq.isEmpty
                    )
               )
        )
        
    override this.Title = "Texture: " + texture_id
    override this.OnClose() = ()

type TextureCard(id: string, on_click: unit -> unit) as this =
    inherit Frame(NodeType.Button (fun () -> Style.click.Play(); on_click()),
        Fill = (fun () -> if this.Focused then Colors.yellow_accent.O1 else Colors.shadow_2.O2),
        Border = (fun () -> if this.Focused then Colors.yellow_accent else Colors.grey_2.O3))

    // todo: refresh on return from editor
    let sprite = getTexture id

    do
        this
        |+ Image(sprite, Position = Position.Margin(20.0f))
        |+ Text(id,
            Align = Alignment.CENTER,
            Position = Position.Margin(Style.PADDING).SliceBottom(25.0f))
        |* Clickable.Focus this

    override this.OnFocus() = Style.hover.Play(); base.OnFocus()