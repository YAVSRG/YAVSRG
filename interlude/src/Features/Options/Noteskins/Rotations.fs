namespace Interlude.Features.OptionsMenu.Noteskins

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.Data.Content
open Interlude.Content
open Interlude.Options
open Interlude.Utils
open Interlude.UI.Menu

type RotationPicker(rotation: Setting<float>) as this =
    inherit StaticContainer(NodeType.Leaf)

    let sprite = get_texture "note"

    let fd () =
        Setting.app (fun x -> (x + 22.5) % 360.0) rotation
        Style.click.Play()

    let bk () =
        Setting.app (fun x -> (x - 22.5) %% 360.0) rotation
        Style.click.Play()

    do
        this
        |+ Text(
            (fun () -> sprintf "%.1f" rotation.Value),
            Position = Position.SliceBottom(30.0f),
            Align = Alignment.LEFT,
            Color = K Colors.text_subheading
        )
        |* Clickable(
            (fun () ->
                (if not this.Selected then
                     this.Select())

                fd ()
            ),
            OnRightClick =
                (fun () ->
                    (if not this.Selected then
                         this.Select())

                    bk ()
                ),
            OnHover =
                fun b ->
                    if b && not this.Focused then
                        this.Focus()
        )

    override this.OnFocus() =
        Style.hover.Play()
        base.OnFocus()

    override this.Draw() =
        if this.Selected then
            Draw.rect this.Bounds Colors.pink_accent.O2
        elif this.Focused then
            Draw.rect this.Bounds Colors.yellow_accent.O2

        Draw.quad
            (Quad.ofRect this.Bounds |> Quad.rotate rotation.Value)
            (Quad.color Color.White)
            (Sprite.pick_texture (3, 0) sprite)

        base.Draw()

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if this.Selected then
            if (%%"up").Tapped() then
                fd ()
            elif (%%"down").Tapped() then
                bk ()
            elif (%%"left").Tapped() then
                bk ()
            elif (%%"right").Tapped() then
                fd ()

type RotationSettingsPage() as this =
    inherit Page()

    let data = Noteskins.Current.config
    let use_rotation = Setting.simple data.UseRotation
    let keycount = Setting.simple options.KeymodePreference.Value
    let receptor_style = Setting.simple data.ReceptorStyle
    let mutable rotations = data.Rotations

    let g keycount i =
        let k = int keycount - 3

        Setting.make (fun v -> rotations.[k].[i] <- v) (fun () -> rotations.[k].[i])
        |> Setting.round 1

    let NOTE_WIDTH = 120.0f

    let _rotations, refresh_rotations =
        refreshable_row
            (fun () -> int keycount.Value)
            (fun i k ->
                let x = -60.0f * float32 k
                let n = float32 i

                RotationPicker(
                    g keycount.Value i,
                    Position =
                        { Position.Default with
                            Left = 0.5f %+ (x + NOTE_WIDTH * n)
                            Right = 0.5f %+ (x + NOTE_WIDTH * n + NOTE_WIDTH)
                        }
                )
            )

    do
        this.Content(
            column ()
            |+ PageSetting("noteskins.edit.userotation", Selector<_>.FromBool(use_rotation))
                .Pos(200.0f)
                .Tooltip(Tooltip.Info("noteskins.edit.userotation"))
            |+ PageSetting(
                "generic.keymode",
                Selector<Keymode>
                    .FromEnum(keycount |> Setting.trigger (ignore >> refresh_rotations))
            )
                .Pos(270.0f)
            |+ PageSetting("noteskins.edit.rotations", _rotations)
                .Pos(370.0f, Viewport.vwidth - 200.0f, NOTE_WIDTH)
            |+ PageSetting(
                "noteskins.edit.receptorstyle",
                Selector(
                    [|
                        ReceptorStyle.Rotate, %"noteskins.edit.receptorstyle.rotate"
                        ReceptorStyle.Flip, %"noteskins.edit.receptorstyle.flip"
                    |],
                    receptor_style
                )
            )
                .Pos(470.0f)
                .Tooltip(Tooltip.Info("noteskins.edit.receptorstyle"))
        )

    override this.Title = %"noteskins.edit.rotations.name"

    override this.OnClose() =
        Noteskins.Current.save_config
            { Noteskins.Current.config with
                Rotations = rotations
                UseRotation = use_rotation.Value
                ReceptorStyle = receptor_style.Value
            }
