namespace Interlude.UI

open System.Drawing
open Percyqaz.Flux.UI
open Interlude.Utils

module Colors =

    let black = Color.Black
    let shadow_1 = Color.FromArgb 0xFF_0c1821
    let shadow_2 = Color.FromArgb 0xFF_122e39

    let white = Color.White
    let grey_1 = Color.FromArgb 0xFF_d9e2e7
    let grey_2 = Color.FromArgb 0xFF_b4c5cc

    let green_accent = Color.FromArgb 0xFF_43ef70
    let green = Color.FromArgb 0xFF_1d9d4b
    let green_shadow = Color.FromArgb 0xFF_0c4b24

    let cyan_accent = Color.FromArgb 0xFF_43e0ef
    let cyan = Color.FromArgb 0xFF_1d869d
    let cyan_shadow = Color.FromArgb 0xFF_084251
    
    let red_accent = Color.FromArgb 0xFF_ef5d57
    let red = Color.FromArgb 0xFF_9c3736
    let red_shadow = Color.FromArgb 0xFF_6e190d

    let pink_accent = Color.FromArgb 0xFF_ff85c0
    let pink = Color.FromArgb 0xFF_bc4980
    let pink_shadow = Color.FromArgb 0xFF_6c2d4d
    
    let blue_accent = Color.FromArgb 0xFF_1a53ff
    let blue = Color.FromArgb 0xFF_001696
    let blue_shadow = Color.FromArgb 0xFF_000450

    let yellow_accent = Color.FromArgb 0xFF_ffd166

[<AutoOpen>]
module ColorExtensions =

    type Color with
        member this.O1 = Color.FromArgb(63, this)
        member this.O2 = Color.FromArgb(127, this)
        member this.O3 = Color.FromArgb(191, this)

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