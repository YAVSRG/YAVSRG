namespace Interlude.Features.Skins

open Percyqaz.Flux.UI
open Prelude.Skins
open Prelude.Skins.Noteskins
open Prelude.Skins.HudLayouts
open Interlude.Content
open Interlude.UI

type TextureActions =

    static member Create(source: Storage, texture_id: string, texture_editor: TextureEditGrid) =
        FlowContainer.Vertical(45.0f)
            .Spacing(Style.PADDING * 3.0f)
            .Position(Position.SliceR(400.0f).Shrink(50.0f))
            .With(

                Button(Icons.ROTATE_CW + " Rotate clockwise", fun () ->
                    for (col, row) in texture_editor.SelectedTextures do
                        source.RotateClockwise((col, row), texture_id) |> ignore

                    source.Reload()
                    texture_editor.Refresh()
                )
                    .Disabled(fun () -> texture_editor.SelectedTextures |> Seq.isEmpty),

                Button(Icons.ROTATE_CCW + " Rotate anticlockwise", fun () ->
                    for (col, row) in texture_editor.SelectedTextures do
                        source.RotateAnticlockwise((col, row), texture_id) |> ignore

                    source.Reload()
                    texture_editor.Refresh()
                )
                    .Disabled(fun () -> texture_editor.SelectedTextures |> Seq.isEmpty),

                Button(Icons.CORNER_LEFT_UP + " Vertical flip", fun () ->
                    for (col, row) in texture_editor.SelectedTextures do
                        source.VerticalFlipTexture((col, row), texture_id) |> ignore

                    source.Reload()
                    texture_editor.Refresh()
                )
                    .Disabled(fun () -> texture_editor.SelectedTextures |> Seq.isEmpty),

                Button(Icons.CORNER_DOWN_LEFT + " Horizontal flip", fun () ->
                    for (col, row) in texture_editor.SelectedTextures do
                        source.HorizontalFlipTexture((col, row), texture_id) |> ignore

                    source.Reload()
                    texture_editor.Refresh()
                )
                    .Disabled(fun () -> texture_editor.SelectedTextures |> Seq.isEmpty),

                Button(Icons.REFRESH_CW + " Cycle selected", fun () ->
                    if source.CycleTextures(texture_editor.SelectedTextures |> Array.ofSeq, texture_id) then
                        source.Reload()
                        texture_editor.Refresh()
                )
                    .Disabled(fun () -> texture_editor.SelectedTextures |> Seq.isEmpty)

            )