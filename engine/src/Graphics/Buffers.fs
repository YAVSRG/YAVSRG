namespace Percyqaz.Flux.Graphics

open OpenTK.Graphics.OpenGL

[<Struct>]
type private Buffer = 
    {
        Target: BufferTarget
        Handle: int
    }

module private Buffer =

    let create (btype: BufferTarget, initial_data: 'T array) : Buffer =
        let handle = GL.GenBuffer()
        GL.BindBuffer(btype, handle)
        GL.BufferData(btype, initial_data.Length * sizeof<'T>, initial_data, BufferUsageHint.DynamicDraw)
        { Target = btype; Handle = handle }

    let vertex_data (data: 'Vertex array, count: int, buffer: Buffer) =
        GL.BufferSubData(buffer.Target, 0, nativeint (count * sizeof<'Vertex>), data)
        
[<Struct>]
type private VertexArrayObject = { Handle: int }

module private VertexArrayObject =

    let create<'Vertex> (vbo: Buffer, ebo: Buffer) : VertexArrayObject =
        let handle = GL.GenVertexArray()
        GL.BindVertexArray handle
        { Handle = handle }

    let vertex_attrib_pointer
        (
            index: int,
            count: int,
            vtype: VertexAttribPointerType,
            normalise: bool,
            vertex_size: int,
            offset: int
        ) =
        GL.VertexAttribPointer(index, count, vtype, normalise, vertex_size, offset)
        GL.EnableVertexAttribArray index
