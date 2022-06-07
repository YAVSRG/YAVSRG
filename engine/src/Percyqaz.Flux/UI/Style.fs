namespace Percyqaz.Flux.UI

open Percyqaz.Common
open Percyqaz.Flux.Utils
open Percyqaz.Flux.Graphics.Fonts

open System.Drawing

module Style =

    let mutable padding = 5.0f
    let mutable baseFont = Unchecked.defaultof<SpriteFont>
    let private accentColor = Animation.Color Color.Blue
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

type Style() =
    
    static member Color (alpha, brightness, white) =
        fun () -> Style.color (alpha, brightness, white)

    static member Color ((alpha, brightness, white), (alpha2, brightness2, white2), bind: Animation.Fade) =
        fun () -> Style.color (
            (lerp bind.Value alpha alpha2) * 255.0f |> int,
            lerp bind.Value brightness brightness2,
            lerp bind.Value white white2 )