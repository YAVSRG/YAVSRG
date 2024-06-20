namespace Percyqaz.Flux.Graphics

open OpenTK.Graphics.OpenGL

module Stencil =
    let mutable private depth = 0

    let start_stencilling (alpha_masking) =
        Batch.draw ()

        if depth = 0 then
            GL.Enable(EnableCap.StencilTest)
            GL.Clear(ClearBufferMask.StencilBufferBit)
            GL.StencilMask(0xFF)
            GL.ColorMask(false, false, false, false)
            Shader.set_uniform_i32 ("alphaMasking", (if alpha_masking then 1 else 0)) Shader.main

        GL.StencilFunc(StencilFunction.Equal, depth, 0xFF)
        GL.StencilOp(StencilOp.Keep, StencilOp.Keep, StencilOp.Incr)
        depth <- depth + 1

    let start_drawing () =
        Batch.draw ()

        GL.ColorMask(true, true, true, true)
        GL.StencilFunc(StencilFunction.Equal, depth, 0xFF)
        GL.StencilOp(StencilOp.Keep, StencilOp.Keep, StencilOp.Keep)

    let finish () =
        Batch.draw ()

        depth <- depth - 1

        if depth = 0 then
            GL.Clear(ClearBufferMask.StencilBufferBit)
            GL.Disable(EnableCap.StencilTest)
            GL.StencilMask(0x00)
            Shader.set_uniform_i32 ("alphaMasking", 0) Shader.main
        else
            GL.StencilFunc(StencilFunction.Lequal, depth, 0xFF)

module Alpha =

    let mutable private mult = 0.0f

    let change_multiplier (m: float32) : float32 =
        if m <> mult then
            Batch.draw()
            Shader.set_uniform_f32 ("alphaMult", m) Shader.main
            let previous_mult = mult
            mult <- m
            previous_mult
        else m