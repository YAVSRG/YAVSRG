namespace Interlude.Graphics

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
        }

    let mutable active = false
    
    let CAPACITY = 64
    let VERTEX_COUNT = CAPACITY * 6 // 2 triangles per quad
    let VERTEX_SIZE = sizeof<Vertex>

    let vertices : Vertex array = Array.zeroCreate VERTEX_COUNT
    let elements : int array = Array.zeroCreate (CAPACITY * 6)

    for i = 0 to CAPACITY - 1 do
        elements.[i * 6 + 0] <- i * 6
        elements.[i * 6 + 1] <- i * 6 + 1
        elements.[i * 6 + 2] <- i * 6 + 2
        elements.[i * 6 + 3] <- i * 6 + 3 
        elements.[i * 6 + 4] <- i * 6 + 4 
        elements.[i * 6 + 5] <- i * 6 + 5
        
    let ebo = Buffer.create BufferTarget.ElementArrayBuffer elements
    let vbo = Buffer.create BufferTarget.ArrayBuffer vertices
    let vao = VertexArrayObject.create<Vertex, int> (vbo, ebo)
        
    // 2 floats in slot 0, for pos
    VertexArrayObject.vertexAttribPointer<float32>(0, 2, VertexAttribPointerType.Float, false, VERTEX_SIZE, 0)
    // 2 floats in slot 1, for uv
    VertexArrayObject.vertexAttribPointer<float32>(1, 2, VertexAttribPointerType.Float, false, VERTEX_SIZE, sizeof<float32> * 2)
    // 4 bytes in slot 2, for color
    VertexArrayObject.vertexAttribPointer<uint8>(2, 4, VertexAttribPointerType.UnsignedByte, true, VERTEX_SIZE, sizeof<float32> * 4)

    let mutable vcount = 0

    let private draw() =
        Buffer.data vertices vbo
        GL.DrawArrays(PrimitiveType.Triangles, 0, vcount)
        vcount <- 0

    let vertex (pos: Vector2) (uv: Vector2) (color: System.Drawing.Color) =
        if vcount = VERTEX_COUNT then draw()
        vertices.[vcount] <-
            { 
                X = pos.X; Y = pos.Y;
                U = uv.X; V = uv.Y;
                R = color.R; G = color.G; B = color.B; A = color.A
            }
        vcount <- vcount + 1

    let start() =
        VertexArrayObject.bind vao
        active <- true

    let finish() =
        draw()
        active <- false