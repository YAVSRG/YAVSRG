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
        mutable last_texture_handle: int
    }
    member this.Capacity : int = this.capacity

    member this.Draw() : unit =
        if this.vcount > 0 then
            GL.BufferSubData(BufferTarget.ArrayBuffer, 0, nativeint (this.vcount * sizeof<Vertex>), this.vertices)
            GL.DrawArrays(PrimitiveType.Triangles, 0, this.vcount)
        this.vcount <- 0

    member this.Vertex(pos: Vector2, uv: Vector2, color: Color, texture_layer: int) : unit =
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

    member inline this.Texture(t: Texture) : unit =
        if this.last_texture_handle <> t.Handle then
            this.Draw()

            if t.TextureUnit = 0 then
                GL.BindTexture(TextureTarget.Texture2DArray, t.Handle)

            Shader.set_uniform_i32 (Shader.sampler_loc, t.TextureUnit)
            this.last_texture_handle <- t.Handle

    member this.Start () : unit =
        this.active <- true

    member this.Finish () : unit =
        this.Draw()
        this.active <- false

    static member Create (capacity: int) : Batch =
        assert(capacity > 0)

        let VERTICES_PER_ELEMENT = 6
        let vertex_count = VERTICES_PER_ELEMENT * capacity

        let vertices: Vertex array = Array.zeroCreate vertex_count
        let elements: int array = Array.init vertex_count id

        let ebo = GL.GenBuffer()
        GL.BindBuffer(BufferTarget.ElementArrayBuffer, ebo)
        GL.BufferData(BufferTarget.ElementArrayBuffer, vertex_count * sizeof<int>, elements, BufferUsageHint.DynamicDraw)

        let vbo = GL.GenBuffer()
        GL.BindBuffer(BufferTarget.ArrayBuffer, vbo)
        GL.BufferData(BufferTarget.ArrayBuffer, vertex_count * sizeof<Vertex>, vertices, BufferUsageHint.DynamicDraw)

        let vao = GL.GenVertexArray()
        GL.BindVertexArray vao

        let VERTEX_SIZE = sizeof<Vertex>
        // 2 floats in slot 0, for pos
        GL.VertexAttribPointer(0, 2, VertexAttribPointerType.Float, false, VERTEX_SIZE, 0)
        GL.EnableVertexAttribArray 0
        // 2 floats in slot 1, for uv
        GL.VertexAttribPointer(1, 2, VertexAttribPointerType.Float, false, VERTEX_SIZE, sizeof<float32> * 2)
        GL.EnableVertexAttribArray 1
        // 4 bytes in slot 2, for color
        GL.VertexAttribPointer(2, 4, VertexAttribPointerType.UnsignedByte, true, VERTEX_SIZE, sizeof<float32> * 4)
        GL.EnableVertexAttribArray 2
        // 1 int in slot 3, for texture layer
        GL.VertexAttribPointer(3, 1, VertexAttribPointerType.Float, false, VERTEX_SIZE, sizeof<float32> * 4 + 4)
        GL.EnableVertexAttribArray 3

        {
            capacity = capacity
            element_buffer = ebo
            vertex_buffer = vbo
            vertex_array_object = vao
            vertices = vertices
            active = false
            vcount = 0
            last_texture_handle = -1
        }