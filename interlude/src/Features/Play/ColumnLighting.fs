namespace Interlude.Features.Play

open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Skins.Noteskins
open Interlude.Options
open Interlude.Content
open Interlude.Features.Play

type ColumnLighting(keys: int, ns: NoteskinConfig, state: PlayState) =
    inherit StaticWidget(NodeType.None)
    let timers = Array.init keys (fun _ ->
        let anim = Animation.Delay ns.ColumnLightDuration
        anim.Update ns.ColumnLightDuration
        anim
    )
    let sprite = Content.Texture "receptorlighting"

    let column_spacing = ns.KeymodeColumnSpacing keys
    let column_width = ns.KeymodeColumnWidth keys
    let column_light_colors = ns.ColumnLightColors.[keys - 3]

    let offset = ns.ColumnLightOffset * column_width

    let column_positions =
        let mutable x = 0.0f

        Array.init
            keys
            (fun i ->
                let v = x

                if i + 1 < keys then
                    x <- x + column_width + column_spacing.[i]

                v
            )

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
                    1.0f - float32 s.Progress |> min 1.0f |> max 0.0f

                let a = 255.0f * percent_remaining |> int

                Render.tex_quad
                    (let x = column_width * 0.5f + column_positions.[k]

                     if options.Upscroll.Value then
                         Sprite.aligned_box_x
                             (this.Bounds.Left + x,
                              this.Bounds.Top + options.HitPosition.Value + offset,
                              0.5f,
                              1.0f,
                              column_width * percent_remaining,
                              -1.0f / percent_remaining)
                             sprite
                     else
                         Sprite.aligned_box_x
                             (this.Bounds.Left + x,
                              this.Bounds.Bottom - options.HitPosition.Value - offset,
                              0.5f,
                              1.0f,
                              column_width * percent_remaining,
                              1.0f / percent_remaining)
                             sprite)
                        .AsQuad
                    (Color.White.O4a a).AsQuad
                    (Sprite.pick_texture (0, column_light_colors.[k]) sprite)

        Array.iteri draw_column timers