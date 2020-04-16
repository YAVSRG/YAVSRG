namespace Interlude.Render

open OpenTK
open OpenTK.Graphics.OpenGL

(*
    Storage of rectangles as left, top, right, bottom
    Preferred over left, top, width, height for a handful of reasons
*)

type Rect = (struct(float32 * float32 * float32 * float32))

module Rect = 

    let create l t r b : Rect = l, t, r, b
    
    let width ((left, _, right, _): Rect) = right - left
    let height ((_, top, _, bottom): Rect) = bottom - top

    let centerX ((left, _, right, _): Rect) = (right + left) * 0.5f
    let centerY ((_, top, _, bottom): Rect) = (bottom + top) * 0.5f
    let center (r: Rect) = (centerX r, centerY r)
    let centerV (r: Rect) = new Vector2(centerX r, centerY r)

    let expand (x,y) ((left, top, right, bottom): Rect) =
        struct (left - x, top - y, right + x, bottom + y)

    let sliceLeft v ((left, top, _, bottom): Rect) =
        struct (left, top, left + v, bottom)

    let sliceTop v ((left, top, right, _): Rect) =
        struct (left, top, right, top + v)

    let sliceRight v ((_, top, right, bottom): Rect) =
        struct (right - v, top, right, bottom)

    let sliceBottom v ((left, _, right, bottom): Rect) =
        struct (left, bottom - v, right, bottom)

(*
    Simple storage of vertices to render as a quad
*)

type Quad = (struct(Vector2 * Vector2 * Vector2 * Vector2))

module Quad =
    
    let ofRect ((l, t, r, b) : Rect) : Quad =
        (new Vector2(l, t), new Vector2(r, t), new Vector2(r, b), new Vector2(l, b))

    let create c1 c2 c3 c4 : Quad = c1, c2, c3, c4

(*
    Render handling to be used from Game
*)

module Render =

    let resize(width, height) =
        GL.MatrixMode(MatrixMode.Projection)
        GL.LoadIdentity()
        GL.Ortho(0.0, width, height, 0.0, 0.0, 1.0)

    let start() = 
        GL.ClearColor(Color.Black)

    let finish() =
        GL.Finish()
        GL.Flush()

    let init(width, height) =
        GL.Enable(EnableCap.Blend)
        GL.Enable(EnableCap.Texture2D)
        GL.Arb.BlendFuncSeparate(0, BlendingFactor.SrcAlpha, BlendingFactor.OneMinusSrcAlpha, BlendingFactor.One, BlendingFactor.OneMinusSrcAlpha)
        GL.ClearStencil(0x00)
        resize(width, height)
        
        //font management here maybe

        //fbo storage here too

(*
    Drawing methods to be used by UI components
*)

module Draw =

    let rect (r: Rect) (c: Color) =
        GL.Begin(PrimitiveType.Quads)
        let struct (p1, p2, p3, p4) = Quad.ofRect r
        GL.Color4(c)
        GL.Vertex2(p1)
        GL.Vertex2(p2)
        GL.Vertex2(p3)
        GL.Vertex2(p4)
        GL.End()
