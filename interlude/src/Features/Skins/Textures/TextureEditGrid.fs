namespace Interlude.Features.Skins

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Prelude
open Prelude.Skins
open Prelude.Skins.Noteskins
open Prelude.Skins.HudLayouts
open Interlude.Content
open Interlude.UI

[<AutoOpen>]
module TextureEditorExtensions =

    type Storage with

        member this.Reload() =
            match this with
            | :? Noteskin as ns -> Skins.reload_current_noteskin()
            | :? HudLayout as hud -> Skins.reload_current_hud()
            | _ -> failwith "Unrecognised storage object (maybe a theme)"

        member this.GetTextureRules(texture_id: string) : TextureRules =
            match this with
            | :? Noteskin as ns -> NoteskinTextureRules.get ns.Config texture_id
            | :? HudLayout as hud -> HudTextureRules.get hud.Config texture_id
            | _ -> failwith "Unrecognised storage object (maybe a theme)"

type private TextureEditGridItem(sprite: Sprite, x: int, y: int, selected: bool array array) =
    inherit
        Container(
            NodeType.Button(fun () ->
                Style.click.Play()
                selected.[x].[y] <- not selected.[x].[y]
            )
        )

    override this.Init(parent) =
        this
        |+ Frame(
            Fill =
                (fun () ->
                    if selected.[x].[y] then Colors.pink_accent.O2
                    elif this.Focused then Colors.yellow_accent.O2
                    else Color.Transparent
                ),
            Border =
                (fun () ->
                    if this.Focused then Colors.white.O3
                    elif selected.[x].[y] then Colors.pink_accent
                    else Color.Transparent
                )
        )
        |* MouseListener()
            .SelectOnClick(this)
            .OnHover(fun now_hovering ->
                if now_hovering then
                    if Mouse.held Mouse.LEFT then
                            this.Select true
                        else
                            this.Focus true
            )

        base.Init parent

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Draw() =
        base.Draw()
        Render.tex_quad this.Bounds.AsQuad Color.White.AsQuad (Sprite.pick_texture (x, y) sprite)

type private DeleteButton(on_click: unit -> unit) =
    inherit Button(Icons.TRASH, on_click, Floating = true)

    member val VerticalPad = 0.0f with get, set

    override this.Draw() =
        if this.Focused then
            Render.rect (this.Bounds.ExpandY(this.VerticalPad)) Colors.yellow_accent.O2

        base.Draw()

type TextureEditGrid(source: Storage, texture_id: string) as this =
    inherit Container(NodeType.Container(fun () -> Some this.Items))

    let rules = source.GetTextureRules texture_id

    let mutable sprite = Unchecked.defaultof<Sprite>
    let mutable selected: bool array array = [||]
    let mutable items: NavigationContainer.Grid = Unchecked.defaultof<_>

    member this.Refresh() =
        sprite <- Content.Texture texture_id

        if sprite.Columns <> selected.Length || sprite.Rows <> selected.[0].Length then
            selected <- Array.init sprite.Columns (fun _ -> Array.zeroCreate sprite.Rows)

        let item_height =
            min
                ((this.Bounds.Width - 10.0f * float32 (sprite.Columns - 1)) / float32 sprite.Columns / sprite.AspectRatio)
                ((this.Bounds.Height - 10.0f * float32 (sprite.Rows - 1)) / float32 sprite.Rows)
        let item_width = item_height * sprite.AspectRatio

        let grid_width =
            item_width * float32 sprite.Columns + 10.0f * float32 (sprite.Columns - 1)

        items <-
            NavigationContainer.Grid()
                .WrapNavigation(false)
                .Floating()
                .Position(Position.Box(0.5f, 0.0f, -grid_width * 0.5f, 0.0f, grid_width, this.Bounds.Height))

        let grid = NavigationContainer.Grid(WrapNavigation = false, Floating = true)

        for r = 0 to sprite.Rows - 1 do
            for c = 0 to sprite.Columns - 1 do

                grid.Add(
                    TextureEditGridItem(
                        sprite,
                        c,
                        r,
                        selected
                    )
                        .Position(Position.Box(0.0f, 0.0f, float32 c * (item_width + 10.0f), float32 r * (item_height + 10.0f), item_width, item_height)),
                    c + 2,
                    r + 2
                )

                if r = 0 then
                    grid.Add(
                        Text(sprintf "Frame %i" (c + 1))
                            .Color(Colors.text_subheading)
                            .Align(Alignment.CENTER)
                            .Position(Position.Box(0.0f, 0.0f, float32 c * (item_width + 10.0f), -90.0f, item_width, 40.0f)),
                        c + 2,
                        0
                    )

                    if sprite.Columns > 1 then
                        grid.Add(
                            DeleteButton(fun () ->
                                ConfirmPage(
                                    sprintf "Really PERMANENTLY delete animation frame %i?" (c + 1),
                                    fun () ->
                                        if source.DeleteLooseTextureColumn(c, texture_id) then
                                            source.Reload()
                                            this.Refresh()
                                )
                                    .Show()
                            )
                                .Position(Position.Box(0.0f, 0.0f, float32 c * (item_width + 10.0f), -50.0f, item_width, 40.0f)),
                            c + 2,
                            1
                        )

            grid.Add(
                Text(sprintf "Color %i" (r + 1))
                    .Color(Colors.text_subheading)
                    .Align(Alignment.RIGHT)
                    .Position(
                        Position
                            .Box(0.0f, 0.0f, -250.0f, float32 r * (item_height + 10.0f), 200.0f, item_height)
                            .Shrink(10.0f, item_height * 0.5f - 20.0f)
                    ),
                0,
                r + 1
            )

            if sprite.Rows > 1 then
                grid.Add(
                    DeleteButton(
                        (fun () ->
                            ConfirmPage(
                                sprintf "Really PERMANENTLY delete color %i?" (r + 1),
                                fun () ->
                                    if source.DeleteLooseTextureRow(r, texture_id) then
                                        source.Reload()
                                        this.Refresh()
                            )
                                .Show()
                        ),
                        VerticalPad = item_height * 0.5f - 20.0f
                    )
                        .Position(
                            Position
                                .Box(0.0f, 0.0f, -50.0f, float32 r * (item_height + 10.0f), 40.0f, item_height)
                                .Shrink(0.0f, item_height * 0.5f - 20.0f)
                        ),
                    1,
                    r + 2
                )

        items.Add(grid, 0, 0)

        if sprite.Rows < fst rules.MaxGridSize then
            items.Add(
                { new Button(
                    K Icons.PLUS_CIRCLE,
                    (fun () ->
                        let src_row =
                            match Seq.tryHead this.SelectedTextures with
                            | Some(x, y) -> y
                            | None -> 0

                        ConfirmPage(
                            sprintf
                                "Add a new color to this texture? (will be a copy of color %i)"
                                (src_row + 1),
                            fun () ->
                            if source.AddLooseTextureRow(src_row, texture_id) then
                                source.Reload()
                                this.Refresh()
                        )
                            .Show()
                    )) with
                    override this.Draw() =
                        if this.Focused then
                            Render.rect this.Bounds Colors.yellow_accent.O2

                        base.Draw()
                }
                    .Floating()
                    .Position(Position.Shrink(0.0f, -50.0f).SliceB(40.0f)),
                0,
                1
            )

        if sprite.Columns < snd rules.MaxGridSize then
            items.Add(
                { new Button(K Icons.PLUS_CIRCLE,
                             (fun () ->
                                 let src_col =
                                     match Seq.tryHead this.SelectedTextures with
                                     | Some(x, y) -> x
                                     | None -> 0

                                 ConfirmPage(
                                     sprintf
                                         "Add a new animation frame to this texture? (will be a copy of frame %i)"
                                         (src_col + 1),
                                     fun () ->
                                        if source.AddLooseTextureColumn(src_col, texture_id) then
                                            source.Reload()
                                            this.Refresh()
                                 )
                                     .Show()
                             ),
                             Floating = true) with
                    override this.Draw() =
                        if this.Focused then
                            Render.rect this.Bounds Colors.yellow_accent.O2

                        base.Draw()
                }.Position(Position.Shrink(-50.0f, 0.0f).SliceR(40.0f)),
                1,
                0
            )

        items.Init this

    member this.SelectedTextures =
        seq {
            for c = 0 to selected.Length - 1 do
                for r = 0 to selected.[c].Length - 1 do
                    if selected.[c].[r] then
                        yield (c, r)
        }

    member private this.Items = items

    override this.Init(parent) =
        base.Init parent
        this.Refresh()

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        items.Update(elapsed_ms, moved)

    override this.Draw() =
        base.Draw()
        items.Draw()