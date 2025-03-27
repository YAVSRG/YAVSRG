namespace Interlude.UI

open System
open System.Globalization
open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude

module NumberEntry =

    let create_int (setting: Setting<int, _>) : FrameContainer =

        let try_parse (s: string) =
            match Int32.TryParse(s, CultureInfo.InvariantCulture) with
            | true, v -> setting.Set v
            | false, _ -> ()

        let text_setting =
            Setting.simple (setting.Value.ToString())
            |> Setting.map id (fun s -> s |> Seq.filter (fun c -> Char.IsAsciiDigit c || c = '-') |> Array.ofSeq |> System.String)

        let entry =
            { new TextEntry(text_setting |> Setting.trigger try_parse, "none", true) with
                override this.OnSelected by_mouse =
                    text_setting.Set ""
                    base.OnSelected by_mouse

                override this.OnFocus by_mouse =
                    base.OnFocus by_mouse
                    Style.hover.Play()

                override this.Update(elapsed_ms, moved) =
                    if not this.Selected then
                        text_setting.Set ((float32 setting.Value).ToString())
                    base.Update(elapsed_ms, moved)
            }

        FrameContainer(NodeType.Container (fun () -> Some entry),
            Fill = K Color.Transparent,
            Border =
                fun () ->
                    if entry.Selected then Colors.pink_accent
                    elif entry.Focused then Colors.yellow_accent
                    else Colors.grey_2
        )
            .Position(Position.ExpandX(15.0f))
        |+ entry.Position(Position.ShrinkX(15.0f))

    let create (setting: Setting<float, _>) : FrameContainer =

        let try_parse (s: string) =
            match Double.TryParse(s, CultureInfo.InvariantCulture) with
            | true, v -> setting.Set v
            | false, _ -> ()

        let text_setting =
            Setting.simple (setting.Value.ToString("R"))
            |> Setting.map id (fun s -> s |> Seq.filter (fun c -> Char.IsAsciiDigit c || c = '.' || c = '-') |> Array.ofSeq |> System.String)

        let entry =
            { new TextEntry(text_setting |> Setting.trigger try_parse, "none", true) with
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
            NodeType.Container(fun () -> Some entry),
            Fill = K Color.Transparent,
            Border =
                fun () ->
                    if entry.Selected then Colors.pink_accent
                    elif entry.Focused then Colors.yellow_accent
                    else Colors.grey_2
        )
            .Position(Position.ExpandX(15.0f))
        |+ entry.Position(Position.ShrinkX 15.0f)

    let create_uom (units: string) (setting: Setting<float32<'u>, _>) : FrameContainer =

        let try_parse (s: string) =
            match Single.TryParse(s, CultureInfo.InvariantCulture) with
            | true, v -> setting.Set (v |> LanguagePrimitives.Float32WithMeasure)
            | false, _ -> ()

        let text_setting =
            Setting.simple ((float32 setting.Value).ToString("R"))
            |> Setting.map id (fun s -> s |> Seq.filter (fun c -> Char.IsAsciiDigit c || c = '.' || c = '-') |> Array.ofSeq |> System.String)

        let entry =
            { new TextEntry(text_setting |> Setting.trigger try_parse, "none", true) with
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
            Fill = K Color.Transparent,
            Border =
                fun () ->
                    if entry.Selected then Colors.pink_accent
                    elif entry.Focused then Colors.yellow_accent
                    else Colors.grey_2
        )
            .Position(Position.ExpandX(15.0f))
        |+ entry.Position(Position.ShrinkX(15.0f).ShrinkR(100.0f))
        |+ Text(units)
            .Color(Colors.text_subheading)
            .Position(Position.SliceR(100.0f))