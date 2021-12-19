namespace Interlude.Graphics

open Prelude.Common
open OpenTK.Graphics.OpenGL

type Buffer = BufferTarget * int

module Buffer =
    
    let create (btype: BufferTarget) (data: 'Vertex array) : Buffer =
        let handle = GL.GenBuffer()
        GL.BindBuffer (btype, handle)
        GL.BufferData (btype, data.Length * sizeof<'Vertex>, data, BufferUsageHint.DynamicDraw)
        (btype, handle)

    let destroy (buf: Buffer) = GL.DeleteBuffer (snd buf)
    
    let bind ((btype, handle): Buffer) = GL.BindBuffer (btype, handle)

    /// Needs to be bound first
    let data (data: 'Vertex array) ((btype, handle): Buffer) =
        GL.BufferData (btype, data.Length * sizeof<'Vertex>, data, BufferUsageHint.DynamicDraw)

type VertexArrayObject = int

module VertexArrayObject =
    
    let create<'Vertex, 'Index> (vbo: Buffer, ebo: Buffer) : VertexArrayObject =
        let handle = GL.GenVertexArray()
        GL.BindVertexArray handle
        Buffer.bind vbo
        Buffer.bind ebo
        handle

    let destroy (vao: VertexArrayObject) =
        GL.DeleteVertexArray vao

    let vertexAttribPointer<'Vertex>(index: int, count: int, vtype: VertexAttribPointerType, normalise: bool, vertexSize: int, offset: int) =
        Logging.Debug (sprintf "ATTRIBUTE %i: %i %Os at offset %i; Total stride %i" index count vtype offset vertexSize)
        GL.VertexAttribPointer (index, count, vtype, normalise, vertexSize * sizeof<'Vertex>, offset * sizeof<'Vertex>)
        GL.EnableVertexAttribArray index

    let bind (vao: VertexArrayObject) =
        GL.BindVertexArray vao