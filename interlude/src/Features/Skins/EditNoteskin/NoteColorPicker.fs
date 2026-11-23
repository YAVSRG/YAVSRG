namespace Interlude.Features.Skins.EditNoteskin

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude
open Prelude.Skins.Noteskins
open Interlude.Content
open Interlude.Options
open Interlude.UI
open Interlude.Features.Gameplay
open Interlude.Features.Skins

type NoteColorPicker(color: Setting<byte>, style: ColorScheme, index: int) =
    inherit Container(NodeType.Leaf)

    let sprite = Content.Texture("note")
    let n = byte sprite.Rows

    let fd () =
        Setting.app (fun x -> (x + 1uy) % n) color
        Style.click.Play()

    let bk () =
        Setting.app (fun x -> (x + n - 1uy) % n) color
        Style.click.Play()

    override this.Init(parent: Widget) : unit =
        this.Add(
            Help(
                Callout.Normal
                    .Title(sprintf "%s: %O" (%"noteskin.notecolors") style)
                    .Body(%(sprintf "noteskin.notecolors.%s.%i" (style.ToString().ToLower()) index))
            ),
            MouseListener()
                .SelectOnClick(this, fd)
                .FocusOnHover(this)
                .OnRightClick(fun () ->
                    this.Select(true)
                    bk()
                )
        )

        base.Init(parent)

    override this.OnFocus(by_mouse: bool) : unit =
        base.OnFocus(by_mouse)
        Style.hover.Play()

    override this.Draw() : unit =
        base.Draw()

        if this.Selected then
            Render.rect this.Bounds Colors.pink_accent.O2
        elif this.Focused then
            Render.rect this.Bounds Colors.yellow_accent.O2

        Render.tex_quad this.Bounds.AsQuad Color.White.AsQuad (Sprite.pick_texture (3, int color.Value) sprite)

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
