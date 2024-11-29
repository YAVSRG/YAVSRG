namespace Percyqaz.Flux.Graphics

open System.Drawing
open System.Runtime.InteropServices
open OpenTK.Graphics.OpenGL
open OpenTK.Mathematics

module Batch =

    [<Struct>]
    [<StructLayout(LayoutKind.Sequential)>]
    type private Vertex =
        {
            X: float32
            Y: float32
            U: float32
            V: float32
            R: uint8
            G: uint8
            B: uint8
            A: uint8
            L: float32
        }

    let mutable private active = false

    let private CAPACITY = 1024
    let private VERTICES_PER_ELEMENT = 6
    let private VERTEX_COUNT = CAPACITY * VERTICES_PER_ELEMENT // 2 triangles per quad
    let private VERTEX_SIZE = sizeof<Vertex>

    let private vertices: Vertex array = Array.zeroCreate VERTEX_COUNT
    let private elements: int array = Array.init VERTEX_COUNT id

    let private ebo = Buffer.create(BufferTarget.ElementArrayBuffer, elements)
    let private vbo = Buffer.create(BufferTarget.ArrayBuffer, vertices)
    let private vao = VertexArrayObject.create (vbo, ebo)

    // 2 floats in slot 0, for pos
    VertexArrayObject.vertex_attrib_pointer (0, 2, VertexAttribPointerType.Float, false, VERTEX_SIZE, 0)
    // 2 floats in slot 1, for uv
    VertexArrayObject.vertex_attrib_pointer (
        1,
        2,
        VertexAttribPointerType.Float,
        false,
        VERTEX_SIZE,
        sizeof<float32> * 2
    )
    // 4 bytes in slot 2, for color
    VertexArrayObject.vertex_attrib_pointer (
        2,
        4,
        VertexAttribPointerType.UnsignedByte,
        true,
        VERTEX_SIZE,
        sizeof<float32> * 4
    )
    // 1 int in slot 3, for texture layer - using an int makes 20 bytes total
    VertexArrayObject.vertex_attrib_pointer (
        3,
        1,
        VertexAttribPointerType.Float,
        false,
        VERTEX_SIZE,
        sizeof<float32> * 4 + 4
    )

    let mutable vcount = 0
    let mutable bcount = 0

    let internal draw () =
        if vcount > 0 then
            Buffer.vertex_data(vertices, vcount, vbo)
            GL.DrawArrays(PrimitiveType.Triangles, 0, vcount)

        vcount <- 0
        bcount <- bcount + 1

    let internal vertex (pos: Vector2) (uv: Vector2) (color: Color) (texture_layer: int) =
        if vcount = VERTEX_COUNT then
            draw ()

        vertices.[vcount] <-
            {
                X = pos.X
                Y = pos.Y
                U = uv.X
                V = uv.Y
                R = color.R
                G = color.G
                B = color.B
                A = color.A
                L = float32 texture_layer
            }

        vcount <- vcount + 1

    let internal start () =
        bcount <- 0
        VertexArrayObject.bind vao
        active <- true

    let internal finish () =
        draw ()
        active <- false