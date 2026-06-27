namespace Prelude

open System
open System.Drawing
open Percyqaz.Common

type Color = Drawing.Color
module [<AutoOpen>] ColorExtensions =

    type Drawing.Color with

        static member FromHsv(H: float32, S: float32, V: float32) : Color =
            let C = V * S
            let X = C * (1.0f - MathF.Abs((H * 6.0f) %% 2.0f - 1.0f))
            let m = V - C

            let r, g, b =
                if H < 1.0f / 6.0f then (C, X, 0.0f)
                elif H < 2.0f / 6.0f then (X, C, 0.0f)
                elif H < 3.0f / 6.0f then (0.0f, C, X)
                elif H < 4.0f / 6.0f then (0.0f, X, C)
                elif H < 5.0f / 6.0f then (X, 0.0f, C)
                else (C, 0.0f, X)

            Color.FromArgb((r + m) * 255.0f |> int, (g + m) * 255.0f |> int, (b + m) * 255.0f |> int)

        /// Doesn't include alpha
        member this.ToHsv() : float32 * float32 * float32 =
            let R = float32 this.R / 255.0f
            let G = float32 this.G / 255.0f
            let B = float32 this.B / 255.0f
            let Cmax = max R G |> max B
            let Cmin = min R G |> min B
            let d = Cmax - Cmin

            let H =
                if d = 0.0f then 0.0f
                elif Cmax = R then (((G - B) / d) %% 6.0f) / 6.0f
                elif Cmax = G then (((B - R) / d) + 2.0f) / 6.0f
                else (((R - G) / d) + 4.0f) / 6.0f

            let S = if Cmax = 0.0f then 0.0f else d / Cmax

            let V = Cmax

            (H, S, V)

        member this.ToHex() : string =
            if this.A = 255uy then
                sprintf "#%02x%02x%02x" this.R this.G this.B
            else
                sprintf "#%02x%02x%02x%02x" this.R this.G this.B this.A

        static member FromHex(s: string) : Color option =
            try
                if s.Length = 9 && s.[0] = '#' then
                    let alpha = Convert.ToByte(s.Substring(7), 16)
                    Color.FromArgb(int alpha, ColorTranslator.FromHtml(s.Substring(0, 7))) |> Some
                elif s.Length = 7 && s.[0] = '#' then
                    ColorTranslator.FromHtml(s) |> Some
                elif s.Length > 0 && s.[0] <> '#' then
                    ColorTranslator.FromHtml(s) |> Some
                else
                    None
            with _ -> None