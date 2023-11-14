namespace Interlude.Features.OptionsMenu.Noteskins

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.Data.Content
open Interlude.Content
open Interlude.Options
open Interlude.Utils
open Interlude.UI.Menu

type HoldNoteSettingsPage() as this =
    inherit Page()

    let data = Noteskins.Current.config

    let hold_note_trim = Setting.bounded data.HoldNoteTrim 0.0f 2.0f |> Setting.roundf 2
    let use_tail_texture = Setting.simple data.UseHoldTailTexture
    let flip_hold_tail = Setting.simple data.FlipHoldTail
    let dropped_color = Setting.simple data.DroppedHoldColor

    let head = get_texture "holdhead"
    let body = get_texture "holdbody"
    let tail = get_texture "holdtail"
    let animation = Animation.Counter(data.AnimationFrameTime)

    do
        this.Content(
            column ()
            |+ PageSetting("noteskins.edit.holdnotetrim", Slider(hold_note_trim))
                .Pos(200.0f)
                .Tooltip(Tooltip.Info("noteskins.edit.holdnotetrim"))
            |+ PageSetting("noteskins.edit.usetailtexture", Selector<_>.FromBool(use_tail_texture))
                .Pos(300.0f)
                .Tooltip(Tooltip.Info("noteskins.edit.usetailtexture"))
            |+ PageSetting("noteskins.edit.flipholdtail", Selector<_>.FromBool(flip_hold_tail))
                .Pos(370.0f)
                .Tooltip(Tooltip.Info("noteskins.edit.flipholdtail"))
            |+ PageSetting("noteskins.edit.droppedholdcolor", ColorPicker(dropped_color, true))
                .Pos(470.0f, PRETTYWIDTH, PRETTYHEIGHT * 2f)
                .Tooltip(Tooltip.Info("noteskins.edit.droppedholdcolor"))
        )

    override this.Draw() =
        base.Draw()

        let COLUMN_WIDTH = 120.0f
        let mutable left = this.Bounds.Right - 50.0f - COLUMN_WIDTH
        let bottom = this.Bounds.Bottom - 100.0f
        let top = this.Bounds.CenterY - 100.0f

        let draw_ln_preview (label: string, color: Color, downscroll: bool) =

            Draw.rect (Rect.Create(left, top, left + COLUMN_WIDTH, bottom)) Colors.black.O2

            let headpos = if downscroll then bottom - COLUMN_WIDTH else top

            let tailpos =
                if downscroll then
                    top + hold_note_trim.Value * COLUMN_WIDTH
                else
                    bottom - COLUMN_WIDTH - hold_note_trim.Value * COLUMN_WIDTH

            Draw.quad
                (Rect.Create(
                    left,
                    min headpos tailpos + COLUMN_WIDTH * 0.5f,
                    left + COLUMN_WIDTH,
                    max headpos tailpos + COLUMN_WIDTH * 0.5f
                 )
                 |> Quad.ofRect)
                (Quad.color color)
                (Sprite.pick_texture (animation.Loops, 0) body)

            Draw.quad
                (Rect.Box(left, headpos, COLUMN_WIDTH, COLUMN_WIDTH) |> Quad.ofRect)
                (Quad.color color)
                (Sprite.pick_texture (animation.Loops, 0) head)

            Draw.quad
                (Rect.Box(left, tailpos, COLUMN_WIDTH, COLUMN_WIDTH)
                 |> if flip_hold_tail.Value && downscroll then
                        fun (r: Rect) -> r.Shrink(0.0f, r.Height)
                    else
                        id
                 |> Quad.ofRect)
                (Quad.color color)
                (Sprite.pick_texture (animation.Loops, 0) (if use_tail_texture.Value then tail else head))

            Text.fill_b (Style.font, label, Rect.Box(left, bottom, COLUMN_WIDTH, 30.0f), Colors.text, Alignment.CENTER)

            left <- left - COLUMN_WIDTH - 50.0f

        draw_ln_preview ("Upscroll", Color.White, false)
        draw_ln_preview ("Dropped", dropped_color.Value, not options.Upscroll.Value)
        draw_ln_preview ("Downscroll", Color.White, true)

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        animation.Update elapsed_ms

    override this.Title = %"noteskins.edit.holdnotes.name"

    override this.OnClose() =
        Noteskins.Current.save_config
            { Noteskins.Current.config with
                HoldNoteTrim = hold_note_trim.Value
                UseHoldTailTexture = use_tail_texture.Value
                FlipHoldTail = flip_hold_tail.Value
                DroppedHoldColor = dropped_color.Value
            }
