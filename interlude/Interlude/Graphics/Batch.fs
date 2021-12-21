namespace Interlude.Graphics

open System.Drawing
open System.Runtime.InteropServices
open OpenTK.Graphics.OpenGL
open OpenTK.Mathematics

module Batch =
    
    [<Struct>]
    [<StructLayout(LayoutKind.Sequential)>]
    type Vertex =
        {
            X: float32; Y: float32
            U: float32; V: float32
            R: uint8; G: uint8; B: uint8; A: uint8
            //T: int32
        }

    let mutable active = false
    
    let CAPACITY = 256
    let VERTICES_PER_ELEMENT = 6
    let VERTEX_COUNT = CAPACITY * VERTICES_PER_ELEMENT // 2 triangles per quad
    let VERTEX_SIZE = sizeof<Vertex>

    let vertices : Vertex array = Array.zeroCreate VERTEX_COUNT
    let elements : int array = Array.init (CAPACITY * VERTICES_PER_ELEMENT) id
        
    let ebo = Buffer.create BufferTarget.ElementArrayBuffer elements
    let vbo = Buffer.create BufferTarget.ArrayBuffer vertices
    let vao = VertexArrayObject.create<Vertex, int> (vbo, ebo)
        
    // 2 floats in slot 0, for pos
    VertexArrayObject.vertexAttribPointer (0, 2, VertexAttribPointerType.Float, false, VERTEX_SIZE, 0)
    // 2 floats in slot 1, for uv
    VertexArrayObject.vertexAttribPointer (1, 2, VertexAttribPointerType.Float, false, VERTEX_SIZE, sizeof<float32> * 2)
    // 4 bytes in slot 2, for color
    VertexArrayObject.vertexAttribPointer (2, 4, VertexAttribPointerType.UnsignedByte, true, VERTEX_SIZE, sizeof<float32> * 4)
    // 1 int in slot 3, for tex unit
    // 4 bytes for padding sake
    //VertexArrayObject.vertexAttribPointer (3, 1, VertexAttribPointerType.Int, false, VERTEX_SIZE, sizeof<float32> * 4 + 4)

    let mutable vcount = 0
    let mutable bcount = 0

    let draw() =
        if vcount > 0 then
            Buffer.data vertices vcount vbo
            GL.DrawArrays(PrimitiveType.Triangles, 0, vcount)
        vcount <- 0
        bcount <- bcount + 1

    let vertex (pos: Vector2) (uv: Vector2) (color: Color) (texUnit: int) =
        if vcount = VERTEX_COUNT then draw()
        vertices.[vcount] <-
            { 
                X = pos.X; Y = pos.Y;
                U = uv.X; V = uv.Y;
                R = color.R; G = color.G; B = color.B; A = color.A
                //T = texUnit
            }
        vcount <- vcount + 1

    let start() =
        bcount <- 0
        VertexArrayObject.bind vao
        active <- true

    let finish() =
        draw()
        //printfn "%i" bcount
        active <- false

module Stencil =
    let mutable depth = 0

    let create(alphaMasking) =
        Batch.draw()

        if depth = 0 then
            GL.Enable(EnableCap.StencilTest)
            GL.Clear(ClearBufferMask.StencilBufferBit)
            GL.StencilMask(0xFF)
            GL.ColorMask(false, false, false, false)
            Shader.setUniformInt ("alphaMasking", if alphaMasking then 1 else 0) Shader.main

        GL.StencilFunc(StencilFunction.Equal, depth, 0xFF)
        GL.StencilOp(StencilOp.Keep, StencilOp.Keep, StencilOp.Incr)
        depth <- depth + 1

    let draw() = 
        Batch.draw()
        
        GL.ColorMask(true, true, true, true)
        GL.StencilFunc(StencilFunction.Equal, depth, 0xFF)
        GL.StencilOp(StencilOp.Keep, StencilOp.Keep, StencilOp.Keep)

    let finish() =
        Batch.draw()

        depth <- depth - 1
        if depth = 0 then
            GL.Clear(ClearBufferMask.StencilBufferBit)
            GL.Disable(EnableCap.StencilTest)
            GL.StencilMask(0x00)
            Shader.setUniformInt ("alphaMasking", 0) Shader.main
        else
            GL.StencilFunc(StencilFunction.Lequal, depth, 0xFF)