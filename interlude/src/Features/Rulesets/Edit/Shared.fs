namespace Interlude.Features.Rulesets.Edit

open System
open System.Globalization
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Interlude.UI

type private ColoredButton(label, color, action) =
    inherit
        Container(
            NodeType.Button(fun _ ->
                Style.click.Play()
                action ()
            )
        )

    override this.Init(parent: Widget) =
        this
        |+ Text(
            K(
                sprintf "%s  >" label
            ),
            Color =
                (fun () ->
                    if this.Focused then Colors.text_yellow_2 else (color, Colors.shadow_2)
                ),
            Align = Alignment.LEFT,
            Position = Position.Shrink(Style.PADDING)
        )
        |* Clickable.Focus this

        base.Init parent

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Draw() =
        if this.Focused then
            Render.rect this.Bounds Colors.yellow_accent.O1

        base.Draw()

module NumberEntry =

    let create (setting: Setting<float, _>) =

        let try_parse (s: string) = 
            match Double.TryParse(s, CultureInfo.InvariantCulture) with
            | true, v -> setting.Set v
            | false, _ -> ()

        let text_setting =
            Setting.simple (setting.Value.ToString("R"))
            |> Setting.map id (fun s -> s |> Seq.filter (fun c -> Char.IsAsciiDigit c || c = '.' || c = '-') |> Array.ofSeq |> String)

        let entry = 
            { new TextEntry(text_setting |> Setting.trigger try_parse, "none", true, Position = Position.ShrinkX 15.0f) with 
                override this.OnSelected by_mouse =
                    text_setting.Set ""
                    base.OnSelected by_mouse

                override this.OnFocus by_mouse =
                    base.OnFocus by_mouse
                    Style.hover.Play()

                override this.Update(elapsed_ms, moved) =
                    if not this.Selected then
                        text_setting.Set ((float32 setting.Value).ToString("R"))
                    base.Update(elapsed_ms, moved)
            }

        FrameContainer(
            NodeType.Container (fun () -> Some entry),
            Position = Position.ExpandX 15.0f,
            Fill = K Color.Transparent,
            Border =
                fun () ->
                    if entry.Selected then Colors.pink_accent
                    elif entry.Focused then Colors.yellow_accent
                    else Colors.grey_2
        )
        |+ entry
    
    let create_uom (units: string) (setting: Setting<float32<'u>, _>) =

        let try_parse (s: string) = 
            match Single.TryParse(s, CultureInfo.InvariantCulture) with
            | true, v -> setting.Set (v |> LanguagePrimitives.Float32WithMeasure)
            | false, _ -> ()

        let text_setting =
            Setting.simple ((float32 setting.Value).ToString("R"))
            |> Setting.map id (fun s -> s |> Seq.filter (fun c -> Char.IsAsciiDigit c || c = '.' || c = '-') |> Array.ofSeq |> String)

        let entry = 
            { new TextEntry(text_setting |> Setting.trigger try_parse, "none", true, Position = Position.ShrinkX(15.0f).ShrinkR 100.0f) with 
                override this.OnSelected by_mouse =
                    text_setting.Set ""
                    base.OnSelected by_mouse

                override this.OnFocus by_mouse =
                    base.OnFocus by_mouse
                    Style.hover.Play()

                override this.Update(elapsed_ms, moved) =
                    if not this.Selected then
                        text_setting.Set ((float32 setting.Value).ToString("R"))
                    base.Update(elapsed_ms, moved)
            }

        FrameContainer(
            NodeType.Container (fun () -> Some entry),
            Position = Position.ExpandX 15.0f,
            Fill = K Color.Transparent,
            Border =
                fun () ->
                    if entry.Selected then Colors.pink_accent
                    elif entry.Focused then Colors.yellow_accent
                    else Colors.grey_2
        )
        |+ entry
        |+ Text(units, Position = Position.SliceR 100.0f, Color = K Colors.text_subheading)