namespace Prelude.Skins.Conversions.Osu.Noteskins

open System.IO
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Processing
open SixLabors.ImageSharp.Drawing.Processing
open Percyqaz.Common
open Prelude
open Prelude.Skins
open Prelude.Skins.Conversions.Osu
            
module internal ColumnLightingConverter =
    
    let convert_column_lighting(ctx: NoteskinConverterContext) =
        try
            let base_image =
                Texture.find (ctx.KeymodeSettings.StageLight, ctx.DefaultSettings.StageLight, ctx.Source)
                |> Texture.expect
                |> Texture.load_single_texture
                |> _.Image

            let distinct_colors = ResizeArray<Color>()

            for k = 0 to ctx.Keymode - 1 do
                let stage_light_color = ctx.KeymodeSettings.ColourLightΔ.[k]

                if not (distinct_colors.Contains stage_light_color) then
                    use colored = base_image.Clone()

                    let color = SixLabors.ImageSharp.Color.FromRgb(stage_light_color.R, stage_light_color.G, stage_light_color.B)
                    colored.Mutate(fun img ->
                        img.Fill(
                            GraphicsOptions(
                                ColorBlendingMode = PixelFormats.PixelColorBlendingMode.Multiply,
                                AlphaCompositionMode = PixelFormats.PixelAlphaCompositionMode.SrcAtop
                            ), color
                        )
                            .Opacity(float32 stage_light_color.A / 255.0f)
                        |> ignore
                    )
                    colored.Save(Path.Combine(ctx.Target, TextureFileName.to_loose "receptorlighting" (0, distinct_colors.Count)))

                    distinct_colors.Add stage_light_color
                ctx.ColumnLightColors.[ctx.Keymode - 3].[k] <- distinct_colors.IndexOf stage_light_color

            ctx.ColumnLights <- true
            let column_width_osu_px = float32 ctx.KeymodeSettings.ColumnWidth.[0]
            let light_position_osu_px = 480.0f - float32 ctx.KeymodeSettings.LightPosition
            let hitposition_osu_px = 480f - float32 ctx.KeymodeSettings.HitPosition
            ctx.ColumnLightsOffset <- (light_position_osu_px - hitposition_osu_px) / column_width_osu_px + 0.5f
        with err ->
            Logging.Warn "Error converting column lighting: %O" err