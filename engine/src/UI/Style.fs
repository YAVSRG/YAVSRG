namespace Percyqaz.Flux.UI

open Percyqaz.Common
open Percyqaz.Flux.Utils
open Percyqaz.Flux.Graphics.Fonts

open System.Drawing

module Style =

    let mutable padding = 5.0f
    let mutable baseFont = Unchecked.defaultof<SpriteFont>
    let accentColor = Animation.Color Color.Blue
    let changePrimaryColor col = accentColor.SetColor col

    let text : unit -> Color * Color = K (Color.White, Color.Black)

    let color (alpha, brightness, white) =
        let accentColor = accentColor.GetColor()
        let r = float32 accentColor.R
        let g = float32 accentColor.G
        let b = float32 accentColor.B
        let rd = (255.0f - r * brightness) * white
        let gd = (255.0f - g * brightness) * white
        let bd = (255.0f - b * brightness) * white
        Color.FromArgb(alpha,
            int ((r + rd) * brightness) |> min 255,
            int ((g + gd) * brightness) |> min 255,
            int ((b + bd) * brightness) |> min 255)

    let highlight (alpha, white) = color (alpha, 1.0f, white)

type PaletteColor =
    {
        Alpha: int
        Brightness: float32
        White: float32
    }
    member this.Lerp (other: PaletteColor) (amount: float32) =
        { 
            Alpha = lerp amount (float32 this.Alpha) (float32 other.Alpha) |> int
            Brightness = lerp amount this.Brightness other.Brightness
            White = lerp amount this.White other.White
        }

module Palette =
    
    let BLACK = { Alpha = 255; Brightness = 0f; White = 0f }
    let WHITE = { Alpha = 255; Brightness = 1f; White = 1f }

    let DARKER = { Alpha = 255; Brightness = 0.25f; White = 0.15f }
    let DARK = { Alpha = 255; Brightness = 0.5f; White = 0.2f }
    let BASE = { Alpha = 255; Brightness = 0.8f; White = 0.25f }
    let LIGHT = { Alpha = 255; Brightness = 1.0f; White = 0.45f }
    let LIGHTER = { Alpha = 255; Brightness = 1.0f; White = 0.7f }

    let HOVER = { LIGHT with Alpha = 127 }
    let SELECTED = { LIGHTER with Alpha = 127 }

    let transition (f: Animation.Fade) (a: PaletteColor) (b: PaletteColor) =
        fun () -> 
            let p = a.Lerp b f.Value
            Style.color (p.Alpha, p.Brightness, p.White)

    let text_transition (f: Animation.Fade) (a: PaletteColor) (b: PaletteColor) =
        fun () -> 
            let p = a.Lerp b f.Value
            Style.color (p.Alpha, p.Brightness, p.White), Color.Black

    let text (a: unit -> Color) (b: unit -> Color) = fun () -> (a(), b())

[<AutoOpen>]
module PaletteOperators =

    let (!%) (p: PaletteColor) =
        if p = Palette.BLACK then K Color.Black
        elif p = Palette.WHITE then K Color.White
        else fun () -> Style.color (p.Alpha, p.Brightness, p.White)
    
    let (!*) (p: PaletteColor) =
        if p = Palette.BLACK then Color.Black
        elif p = Palette.WHITE then Color.White
        else Style.color (p.Alpha, p.Brightness, p.White)
    
    let (!^) (p: PaletteColor) =
        fun () -> Style.color (p.Alpha, p.Brightness, p.White), Color.Black