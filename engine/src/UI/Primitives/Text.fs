namespace Percyqaz.Flux.UI

open System.Drawing
open System.Runtime.CompilerServices
open Percyqaz.Common
open Percyqaz.Flux.Graphics

[<Sealed>]
type Text(text_func: unit -> string) =
    inherit StaticWidget(NodeType.None)

    new(text: string) = Text(K text)

    member val Align : float32 = Alignment.CENTER with get, set
    member val Color : unit -> Color * Color = K Colors.text with get, set

    override this.Draw() =
        Text.fill_b (Style.font, text_func (), this.Bounds, this.Color(), this.Align)

    override this.Init(parent: Widget) = base.Init parent

[<Extension>]
type TextExtensions =

    [<Extension>]
    static member Align (text: Text, alignment: float32) : Text =
        text.Align <- alignment
        text

    [<Extension>]
    static member Color (text: Text, color: Color * Color) : Text =
        text.Color <- K color
        text

    [<Extension>]
    static member Color (text: Text, fg: Color, bg: Color) : Text =
        text.Color <- K (fg, bg)
        text

    [<Extension>]
    static member Color (text: Text, color: Color) : Text =
        text.Color <- K (color, Colors.shadow_2)
        text

    [<Extension>]
    static member Color (text: Text, color: unit -> Color * Color) : Text =
        text.Color <- color
        text

    [<Extension>]
    static member Color (text: Text, color: unit -> Color) : Text =
        text.Color <- fun () -> (color(), Colors.shadow_2)
        text