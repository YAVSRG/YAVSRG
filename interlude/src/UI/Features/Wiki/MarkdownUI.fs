namespace Interlude.Features.Wiki

open System.Drawing
open System.Diagnostics
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Interlude
open Interlude.UI
open Interlude.Utils
open FSharp.Formatting.Markdown

module MarkdownUI =

    let SIZE = 25.0f
    let WIDTH = 1200.0f

    type Fragment =
        {
            Body: StaticContainer
            mutable LHeight: float32
            mutable LWidth: float32
            mutable Height: float32 // not including last line
            mutable Width: float32
        }
    module Fragment =
        let addTo (parent: Fragment) (child: Fragment) =
            let childTotalHeight = child.Height + child.LHeight
            parent.LHeight <- max parent.LHeight childTotalHeight
            child.Body.Position <- Position.Box (0.0f, 0.0f, parent.LWidth, parent.Height, child.Width, childTotalHeight)
            parent.Body.Add child.Body
            parent.LWidth <- parent.LWidth + child.Width
            parent.Width <- max parent.LWidth parent.Width
            parent

        let newline (w: Fragment) =
            w.Height <- w.Height + w.LHeight
            w.LWidth <- 0.0f
            w.LHeight <- 0.0f
            w

        let addToNL (parent: Fragment) (child: Fragment) = addTo parent child |> newline

        let pad (x, y) (w: Fragment) =
            w.LWidth <- w.LWidth + x
            w.Height <- w.Height + y
            w

        let make (w: Widget) (width: float32, height: float32) =
            {
                Body = StaticContainer(NodeType.None) |+ w
                LHeight = height
                LWidth = width
                Height = 0.0f
                Width = width
            }

        let text (str: string) (col: Color * Color) (size: float32) =
            let width = (Text.measure(Content.font, str)) * size
            make <| Text(str, Color = K col, Align = Alignment.LEFT) <| (width, size / 0.6f)

        let sym str = text str (Color.Silver, Color.Red) 25.0f

        let empty() = make (Dummy()) (0.0f, 0.0f)

    type SpanSettings =
        {
            Size: float32
            Bold: bool
            Italic: bool
            HasLink: bool
        }
        static member Default = { Size = SIZE; Bold = false; Italic = false; HasLink = false }

    let openlink (str: string) =
        try Process.Start (ProcessStartInfo (str, UseShellExecute=true)) |> ignore
        with err -> Logging.Debug ("Failed to open link: " + str, err)

    let rec span (settings: SpanSettings) (sp: MarkdownSpan) : Fragment =
        match sp with
        | Literal (text, _) ->
            Fragment.text text
                ((if settings.Bold then Style.color(255, 1.0f, 0.3f) else Color.White),
                    if settings.Italic then Color.Gray 
                    elif settings.HasLink then Color.Blue
                    else Color.Black)
                settings.Size
        | InlineCode (code, _) -> Fragment.text code (Color.Silver, Color.Gray) settings.Size
        | Strong (body, _) -> spans { settings with Bold = true } body
        | Emphasis (body, _) -> spans { settings with Italic = true } body
        | AnchorLink (link, _) -> Fragment.sym "anchorLink"
        | DirectLink (body, link, title, _) ->
            let r = spans { settings with HasLink = true } body
            r.Body.Add (Percyqaz.Flux.UI.Clickable(fun () -> openlink link))
            r
        | IndirectLink (body, link, title, _) -> Fragment.sym "ilink"
        | DirectImage (body, link, title, _) -> Fragment.sym "dimg"
        | IndirectImage (body, link, title, _) -> Fragment.sym "iimg"
        | HardLineBreak _ -> Fragment.sym "linebreak"
        | EmbedSpans _
        | LatexDisplayMath _
        | LatexInlineMath _ -> Fragment.empty()

    and spans settings (sps: MarkdownSpans) : Fragment =
        let block = Fragment.empty()
        List.map (span settings) sps
        |> List.fold Fragment.addTo block

    and addParagraph (body: Fragment) (p: MarkdownParagraph) =
        match p with
        | Heading (size, body, _) -> 
            let b = Fragment.empty() |> Fragment.pad (0.0f, 15.0f)
            Fragment.addTo b (spans { SpanSettings.Default with Size = (SIZE + 5.0f * (4.0f - float32 size)) } body)
            |> Fragment.pad (0.0f, 15.0f)
        | Paragraph (body, _) -> spans SpanSettings.Default body
        | Span (body, _) -> spans SpanSettings.Default body
        | ListBlock (kind, items, _) ->
            let list = Fragment.empty()
            let bullet() = 
                let b = Fragment.empty() |> Fragment.pad(20.0f, 0.0f)
                Fragment.addTo b (Fragment.text "•" (Color.White, Color.Transparent) SIZE)
                |> Fragment.pad(10.0f, 0.0f)
            items
            |> List.map 
                ( 
                    fun i -> 
                        let block = bullet()
                        Fragment.addTo block (i |> List.fold addParagraph (Fragment.empty()))
                )
            |> List.fold Fragment.addToNL list
        | HorizontalRule (char, _) -> Fragment.sym "rule"
        | YamlFrontmatter _
        | TableBlock _
        | OutputBlock _
        | OtherBlock _
        | LatexBlock _
        | QuotedBlock _
        | CodeBlock _
        | EmbedParagraphs _
        | InlineHtmlBlock _ -> Fragment.empty()
        |> Fragment.addToNL body

    let build (doc: MarkdownDocument) : Fragment =
        doc.Paragraphs
        |> List.fold addParagraph (Fragment.empty())