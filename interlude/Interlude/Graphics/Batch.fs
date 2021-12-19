namespace Interlude.Graphics

open OpenTK.Graphics.OpenGL
open OpenTK.Mathematics

module Batch =
    
    let mutable active = false
    
    let VERTEX_COUNT = 4 // 4 corners of quad
    let VERTEX_SIZE = 4 // 2 for pos, 2 for uv
    let vertices : float32 array = Array.zeroCreate (VERTEX_COUNT * VERTEX_SIZE)
    let elements = [|0; 1; 2; 2; 3; 0|]
        
    let ebo = Buffer.create BufferTarget.ElementArrayBuffer elements
    let vbo = Buffer.create BufferTarget.ArrayBuffer vertices
    let vao = VertexArrayObject.create<float32, int> (vbo, ebo)
        
    // 3 floats in shader slot 0, offset 0 for pos
    VertexArrayObject.vertexAttribPointer(0, 2, VertexAttribPointerType.Float, VERTEX_SIZE, 0)
    // 2 floats in shader slot 1, offset 2 for uv
    VertexArrayObject.vertexAttribPointer(1, 2, VertexAttribPointerType.Float, VERTEX_SIZE, 2)

    let vertex i (pos: Vector2) (uv: Vector2) =
        vertices.[i * VERTEX_SIZE] <- pos.X
        vertices.[i * VERTEX_SIZE + 1] <- pos.Y
        vertices.[i * VERTEX_SIZE + 2] <- uv.X
        vertices.[i * VERTEX_SIZE + 3] <- uv.Y

    let private draw() =
        Buffer.data vertices vbo
        GL.DrawArrays(PrimitiveType.Triangles, 0, 4)

    let start() =
        VertexArrayObject.bind vao
        active <- true

    let finish() =
        draw()
        active <- false