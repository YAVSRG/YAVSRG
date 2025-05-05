namespace Interlude.Features.Skins.EditNoteskin

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Skins.Noteskins
open Interlude.Content
open Interlude.Options
open Interlude.UI

type HoldNoteSettingsPage() =
    inherit Page()

    let data = Content.NoteskinConfig

    let hold_note_trim = data.HoldNoteTrim |> Setting.bounded (-1.0f, 2.0f) |> Setting.roundf 2
    let hold_note_fade = data.HoldNoteFade |> Setting.bounded (0.0f, 5.0f) |> Setting.roundf 2
    let minimum_hold_note_length = Setting.simple data.MinimumHoldNoteLength
    let use_tail_texture = Setting.simple data.UseHoldTailTexture
    let flip_hold_tail = Setting.simple data.FlipHoldTail
    let dropped_color = Setting.simple data.DroppedHoldColor

    let head = Content.Texture "holdhead"
    let body = Content.Texture "holdbody"
    let tail = Content.Texture "holdtail"
    let animation = Animation.Counter(float data.AnimationFrameTime)

    override this.Content() =
        page_container()
        |+ PageSetting(%"noteskin.holdnotetrim", Slider.Percent(hold_note_trim))
            .Help(Help.Info("noteskin.holdnotetrim"))
            .Pos(0)
        |+ PageSetting(%"noteskin.holdnotefade", Slider.Percent(hold_note_fade))
            .Help(Help.Info("noteskin.holdnotefade"))
            .Pos(2)
        |+ PageSetting(%"noteskin.minimum_hold_note_length", Checkbox minimum_hold_note_length)
            .Help(Help.Info("noteskin.minimum_hold_note_length"))
            .Pos(4)
        |+ PageSetting(%"noteskin.usetailtexture", Checkbox use_tail_texture)
            .Help(Help.Info("noteskin.usetailtexture"))
            .Pos(7)
        |+ PageSetting(%"noteskin.flipholdtail", Checkbox flip_hold_tail)
            .Help(Help.Info("noteskin.flipholdtail"))
            .Conditional(use_tail_texture.Get)
            .Pos(9)
        |+ PageSetting(%"noteskin.droppedholdcolor", ColorPicker(%"noteskin.droppedholdcolor", dropped_color, false))
            .Help(Help.Info("noteskin.droppedholdcolor"))
            .Pos(12)
        :> Widget

    override this.Draw() =
        base.Draw()

        let COLUMN_WIDTH = 120.0f
        let mutable left = this.Bounds.Right - 50.0f - COLUMN_WIDTH
        let bottom = this.Bounds.Bottom - 100.0f
        let top = this.Bounds.CenterY - 100.0f

        let draw_ln_preview (label: string, color: Color, downscroll: bool) =

            Render.rect_edges left top (left + COLUMN_WIDTH) bottom data.PlayfieldColor

            let headpos = if downscroll then bottom - COLUMN_WIDTH else top

            let tailpos =
                if downscroll then
                    top + hold_note_trim.Value * COLUMN_WIDTH
                else
                    bottom - COLUMN_WIDTH - hold_note_trim.Value * COLUMN_WIDTH

            Render.tex_quad
                (Rect
                    .FromEdges(
                        left,
                        min headpos tailpos + COLUMN_WIDTH * 0.5f - 1f,
                        left + COLUMN_WIDTH,
                        max headpos tailpos + COLUMN_WIDTH * 0.5f + 1f
                    )
                    .AsQuad)
                color.AsQuad
                (Sprite.pick_texture (animation.Loops, 0) body)

            //draw fade
            Render.quad_c
                (Rect.FromSize(
                    left, 
                    tailpos + COLUMN_WIDTH * 0.5f - (if downscroll then 1.0f else -1.0f), 
                    COLUMN_WIDTH, 
                    COLUMN_WIDTH * hold_note_fade.Value * (if downscroll then 1.0f else -1.0f))
                ).AsQuad
                (Quad.gradient_top_to_bottom (Color.FromArgb(255, data.PlayfieldColor)) (Color.FromArgb(0, 0, 0, 0)))

            Render.tex_quad
                (Rect.FromSize(left, headpos, COLUMN_WIDTH, COLUMN_WIDTH).AsQuad)
                color.AsQuad
                (Sprite.pick_texture (animation.Loops, 0) head)

            if hold_note_fade.Value < 0.01f then
                Render.tex_quad
                    (Rect.FromSize(left, tailpos, COLUMN_WIDTH, COLUMN_WIDTH)
                        |> if flip_hold_tail.Value && downscroll && use_tail_texture.Value then
                                fun (r: Rect) -> r.ShrinkY(r.Height)
                            else
                                id)
                            .AsQuad
                    color.AsQuad
                    (Sprite.pick_texture (animation.Loops, 0) (if use_tail_texture.Value then tail else head))

            Text.fill_b (Style.font, label, Rect.FromSize(left, bottom, COLUMN_WIDTH, 30.0f), Colors.text, Alignment.CENTER)

            left <- left - COLUMN_WIDTH - 50.0f

        draw_ln_preview ("Upscroll", Color.White, false)
        draw_ln_preview ("Dropped", dropped_color.Value, not options.Upscroll.Value)
        draw_ln_preview ("Downscroll", Color.White, true)

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        animation.Update elapsed_ms

    override this.Title = %"noteskin.holdnotes"

    override this.OnClose() =
        Skins.save_noteskin_config
            { Content.NoteskinConfig with
                HoldNoteTrim = hold_note_trim.Value
                HoldNoteFade = hold_note_fade.Value
                MinimumHoldNoteLength = minimum_hold_note_length.Value
                UseHoldTailTexture = use_tail_texture.Value
                FlipHoldTail = flip_hold_tail.Value
                DroppedHoldColor = dropped_color.Value
            }