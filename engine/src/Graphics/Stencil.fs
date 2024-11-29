namespace Percyqaz.Flux.Graphics

open OpenTK.Graphics.OpenGL

module Alpha =

    let mutable private mult = 0.0f

    let change_multiplier (m: float32) : float32 =
        if m <> mult then
            Batch.draw()
            Shader.set_uniform_f32 (Shader.alpha_mult_loc, m)
            let previous_mult = mult
            mult <- m
            previous_mult
        else m