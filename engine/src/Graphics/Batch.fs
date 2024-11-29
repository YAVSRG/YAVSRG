namespace Percyqaz.Flux.Graphics

open System.Drawing
open System.Runtime.InteropServices
open OpenTK.Graphics.OpenGL
open OpenTK.Mathematics

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

type internal Batch =
    private {
        capacity: int
        element_buffer: int
        vertex_buffer: int
        vertex_array_object: int
        vertices: Vertex array
        mutable active: bool
        mutable vcount: int
    }
    member this.Capacity = this.capacity

    member this.Draw() =
        if this.vcount > 0 then
            GL.BufferSubData(BufferTarget.ArrayBuffer, 0, nativeint (this.vcount * sizeof<Vertex>), this.vertices)
            GL.DrawArrays(PrimitiveType.Triangles, 0, this.vcount)
        this.vcount <- 0

    member this.Vertex(pos: Vector2, uv: Vector2, color: Color, texture_layer: int) =
        if this.vcount = this.vertices.Length then
            this.Draw()

        this.vertices.[this.vcount] <-
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

        this.vcount <- this.vcount + 1
    
    member this.Start () =
        this.active <- true

    member this.Finish () =
        this.Draw()
        this.active <- false

    static member Create (capacity: int) =
        let VERTICES_PER_ELEMENT = 6
        let vertex_count = VERTICES_PER_ELEMENT * capacity

        let vertices: Vertex array = Array.zeroCreate vertex_count
        let elements: int array = Array.init vertex_count id

        let ebo = Buffer.create(BufferTarget.ElementArrayBuffer, elements)
        let vbo = Buffer.create(BufferTarget.ArrayBuffer, vertices)
        let vao = VertexArrayObject.create (vbo, ebo)

        let VERTEX_SIZE = sizeof<Vertex>
        // 2 floats in slot 0, for pos
        VertexArrayObject.vertex_attrib_pointer
            (0, 2, VertexAttribPointerType.Float, false, VERTEX_SIZE, 0)
        // 2 floats in slot 1, for uv
        VertexArrayObject.vertex_attrib_pointer 
            (1, 2, VertexAttribPointerType.Float, false, VERTEX_SIZE, sizeof<float32> * 2)
        // 4 bytes in slot 2, for color
        VertexArrayObject.vertex_attrib_pointer
            (2, 4, VertexAttribPointerType.UnsignedByte, true, VERTEX_SIZE, sizeof<float32> * 4)
        // 1 int in slot 3, for texture layer - using an int makes 20 bytes total
        VertexArrayObject.vertex_attrib_pointer 
            (3, 1, VertexAttribPointerType.Float, false, VERTEX_SIZE, sizeof<float32> * 4 + 4)

        {
            capacity = capacity
            element_buffer = ebo.Handle
            vertex_buffer = vbo.Handle
            vertex_array_object = vao.Handle
            vertices = vertices
            active = false
            vcount = 0
        }