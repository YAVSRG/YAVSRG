namespace Interlude.Features.Play

open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Skinning.Noteskins
open Interlude.Options
open Interlude.Content
open Interlude.Features.Play

type ColumnLighting(keys, ns: NoteskinConfig, state) as this =
    inherit StaticWidget(NodeType.None)
    let timers = Array.init keys (fun _ -> Animation.Delay ns.ColumnLightDuration)
    let sprite = Content.Texture "receptorlighting"

    let column_spacing = ns.KeymodeColumnSpacing keys

    let column_positions =
        let mutable x = 0.0f

        Array.init
            keys
            (fun i ->
                let v = x

                if i + 1 < keys then
                    x <- x + ns.ColumnWidth + column_spacing.[i]

                v
            )

    do
        let hitpos = float32 options.HitPosition.Value

        this.Position <-
            { Position.Default with
                Top = 0.0f %+ hitpos
                Bottom = 1.0f %- hitpos
            }

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        timers |> Array.iter (fun s -> s.Update elapsed_ms)

        Array.iteri
            (fun k (s: Animation.Delay) ->
                if state.Scoring.KeyState |> Bitmask.has_key k then
                    s.Reset()
            )
            timers

    override this.Draw() =

        let draw_column k (s: Animation.Delay) =
            if not s.Complete then
                let percent_remaining =
                    1.0f - float32 (s.Elapsed / s.Interval) |> min 1.0f |> max 0.0f

                let a = 255.0f * percent_remaining |> int

                Draw.quad
                    (let x = ns.ColumnWidth * 0.5f + column_positions.[k]

                     if options.Upscroll.Value then
                         Sprite.aligned_box_x
                             (this.Bounds.Left + x,
                              this.Bounds.Top,
                              0.5f,
                              1.0f,
                              ns.ColumnWidth * percent_remaining,
                              -1.0f / percent_remaining)
                             sprite
                     else
                         Sprite.aligned_box_x
                             (this.Bounds.Left + x,
                              this.Bounds.Bottom,
                              0.5f,
                              1.0f,
                              ns.ColumnWidth * percent_remaining,
                              1.0f / percent_remaining)
                             sprite)
                        .AsQuad
                    (Color.White.O4a a).AsQuad
                    (Sprite.pick_texture (0, k) sprite)

        Array.iteri draw_column timers