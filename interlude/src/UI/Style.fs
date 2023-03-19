namespace Interlude.UI

open System.Drawing
open Percyqaz.Flux.UI
open Interlude.Utils

module Colors =
    
    let green2 = Color.FromArgb 0xFF33FF66
    let green3 = Color.FromArgb 0xFF5CFF85
    let green4 = Color.FromArgb 0xFF89FFA6
    let yellow = Color.FromArgb 0xFFFFD166
    let pink = Color.FromArgb 0xFFFF85C0
    let blue = Color.FromArgb 0xFF0078BE

    let black = Color.Black
    let white = Color.White
    let grey1 = Color.FromArgb 0xFF0C1821

// todo: merge into Percyqaz.Flux
module Style =

    open Percyqaz.Flux.UI.Style

    type private ColorFunc = unit -> Color
    type private ColorFuncF = float32 -> Color

    let alpha a (c: 'T -> Color) = fun x -> Color.FromArgb(a, c x)
    let alphaF (a: Animation.Fade) (c: ColorFunc) = fun () -> Color.FromArgb(a.Alpha, c ())

    let white a : ColorFunc = K <| Color.FromArgb(a, Color.White)
    let black a : ColorFunc = K <| Color.FromArgb(a, Color.Black)

    /// 1.0, 0.0
    let highlight a : ColorFunc = fun () ->
        color (a, 1.0f, 0.0f)
        
    /// 1.0, 0.5
    let highlightL a : ColorFunc = fun () ->
        color (a, 1.0f, 0.5f)

    /// 0.25, 0.0
    let highlightD a : ColorFunc = fun () ->
        color (a, 0.25f, 0.0f)

    /// 1.0f, f
    let highlightF a : ColorFuncF = fun f ->
        color (a, 1.0f, f)
        
    /// 0.9, 0.0
    let main a : ColorFunc = fun () ->
        color (a, 0.9f, 0.0f)
                
    /// 0.9, f
    let mainF a : ColorFuncF = fun f ->
        color (a, 0.9f, f)
    
    /// 0.25, 0.5
    let dark a : ColorFunc = fun () ->
        color (a, 0.5f, 0.2f)
    
    /// 0.25, 0.75
    let darkL a : ColorFunc = fun () ->
        color (a, 0.25f, 0.75f)
    
    /// 0.1, 0.25
    let darkD a : ColorFunc = fun () ->
        color (a, 0.1f, 0.25f)