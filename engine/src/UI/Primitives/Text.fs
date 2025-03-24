namespace Percyqaz.Flux.UI

open System.Drawing
open System.Runtime.CompilerServices
open Percyqaz.Common
open Percyqaz.Flux.Graphics

[<Sealed>]
type Text(text_func: unit -> string) =
    inherit StaticWidget(NodeType.None)

    new(text: string) = Text(K text)

    member val Align = Alignment.CENTER with get, set
    member val Color = K Colors.text with get, set

    override this.Draw() =
        Text.fill_b (Style.font, text_func (), this.Bounds, this.Color(), this.Align)

    override this.Init(parent) = base.Init parent

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
    static member Color (text: Text, color: unit -> Color * Color) : Text =
        text.Color <- color
        text