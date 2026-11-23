namespace Interlude.Features.Skins.EditNoteskin

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude
open Interlude.Content

type NoteRotationPicker(rotation: Setting<float>) as this =
    inherit Container(NodeType.Leaf)

    let sprite = Content.Texture("note")

    let fd () =
        Setting.app (fun x -> (x + 22.5) % 360.0) rotation
        Style.click.Play()

    let bk () =
        Setting.app (fun x -> (x - 22.5) %% 360.0) rotation
        Style.click.Play()

    do
        this.Add(
            Text(fun () -> sprintf "%.1f" rotation.Value)
                .Position(Position.SliceB(30.0f))
                .Align(Alignment.LEFT)
                .Color(Colors.text_subheading),
            MouseListener()
                .SelectOnClick(this, fd)
                .FocusOnHover(this)
                .OnRightClick(fun () ->
                    this.Select(true)
                    bk()
                )
        )

    override this.OnFocus(by_mouse: bool) : unit =
        base.OnFocus(by_mouse)
        Style.hover.Play()

    override this.Draw() : unit =
        if this.Selected then
            Render.rect this.Bounds Colors.pink_accent.O2
        elif this.Focused then
            Render.rect this.Bounds Colors.yellow_accent.O2

        Render.tex_quad
            (this.Bounds.AsQuad |> Quad.rotate rotation.Value)
            Color.White.AsQuad
            (Sprite.pick_texture (3, 0) sprite)

        base.Draw()

    override this.Update(elapsed_ms: float, moved: bool) : unit =
        base.Update(elapsed_ms, moved)

        if this.Selected then
            if (%%"up").Pressed() then
                fd()
            elif (%%"down").Pressed() then
                bk()
            elif (%%"left").Pressed() then
                bk()
            elif (%%"right").Pressed() then
                fd()
