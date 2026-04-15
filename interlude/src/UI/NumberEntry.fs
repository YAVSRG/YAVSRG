namespace Interlude.UI

open System
open System.Globalization
open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude

type NumberEntry =

    static let UNITS_LABEL_WIDTH = 100.0f
    static member private CreateInternal<'T>(char_allowed: char -> bool, to_string: unit -> string, from_string: string -> unit, units: string option) : Container =
        let text_setting =
            Setting.simple (to_string())
            |> Setting.map id (fun s -> s |> Seq.filter char_allowed |> Array.ofSeq |> String)

        let entry =
            { new TextEntry(text_setting |> Setting.trigger from_string, "none", true) with
                override this.OnSelected by_mouse =
                    text_setting.Set ""
                    base.OnSelected by_mouse

                override this.OnFocus by_mouse =
                    base.OnFocus by_mouse
                    Style.hover.Play()

                override this.Update(elapsed_ms, moved) =
                    if not this.Selected then
                        text_setting.Set (to_string())
                    base.Update(elapsed_ms, moved)
            }

        let container =
            Container.Create(
                entry
                    .Position(Position.ShrinkR(if units.IsSome then UNITS_LABEL_WIDTH else 0.0f))
            )
                .With(
                    Frame()
                        .Fill(Color.Transparent)
                         .Border(fun () ->
                             if entry.Selected then Colors.pink_accent
                             elif entry.Focused then Colors.yellow_accent
                             else Colors.grey_2
                         )
                        .Position(Position.ExpandX(Style.PADDING * 3.0f))
                )
        match units with
        | Some u ->
            container
                .With(
                    Text(u)
                        .Align(Alignment.RIGHT)
                        .Color(Colors.text_subheading)
                        .Position(Position.SliceR(UNITS_LABEL_WIDTH))
                )
        | None -> container

    static member private CreateInternal<'T>(char_allowed: char -> bool, to_string: unit -> string, from_string: string -> unit) : Container =
        NumberEntry.CreateInternal(char_allowed, to_string, from_string, None)

    static member Create(setting: Setting<int, _>) : Container =
        let from_string (text: string) : unit =
            match Int32.TryParse(text, CultureInfo.InvariantCulture) with
            | true, v -> setting.Set v
            | false, _ -> ()
        let to_string () = setting.Value.ToString(CultureInfo.InvariantCulture)

        NumberEntry.CreateInternal((fun c -> Char.IsAsciiDigit c || c = '-'), to_string, from_string)

    static member Create(setting: Setting<float, _>) : Container =
        let from_string (text: string) : unit =
            match Double.TryParse(text, CultureInfo.InvariantCulture) with
            | true, v -> setting.Set v
            | false, _ -> ()
        let to_string () : string = setting.Value.ToString("R", CultureInfo.InvariantCulture)

        NumberEntry.CreateInternal((fun c -> Char.IsAsciiDigit c || c = '-' || c = '.'), to_string, from_string)
        
    static member CreatePercent(setting: Setting<float, _>) : Container =
        let from_string (text: string) : unit =
            match Double.TryParse(text, CultureInfo.InvariantCulture) with
            | true, v -> setting.Set (v / 100.0)
            | false, _ -> ()
        let to_string () : string = (setting.Value * 100.0).ToString("R", CultureInfo.InvariantCulture)

        NumberEntry.CreateInternal((fun c -> Char.IsAsciiDigit c || c = '-' || c = '.'), to_string, from_string, Some "%")
        
    static member Create(setting: Setting<float32<'u>, _>, units_label: string) : Container =
        let from_string (text: string) : unit =
            match Single.TryParse(text, CultureInfo.InvariantCulture) with
            | true, v -> setting.Set (v |> LanguagePrimitives.Float32WithMeasure)
            | false, _ -> ()
        let to_string () : string = (float32 setting.Value).ToString("R", CultureInfo.InvariantCulture)

        NumberEntry.CreateInternal((fun c -> Char.IsAsciiDigit c || c = '-' || c = '.'), to_string, from_string, Some units_label)

    static member Create(setting: Setting<Color>) : Container =
        let from_string (text: string) : unit =
            match Color.FromHex(text) with
            | Some c -> setting.Set c
            | None -> ()
        let to_string () : string = setting.Value.ToHex()
        NumberEntry.CreateInternal(K true, to_string, from_string)