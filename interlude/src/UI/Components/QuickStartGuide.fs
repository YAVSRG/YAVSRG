namespace Interlude.UI.Components

open System.Drawing
open System.Diagnostics
open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Interlude
open Interlude.UI
open Interlude.Utils

// todo: this is not a component
// todo: split into a folder of parts (markdown, wiki logic etc(
// todo: make this read from/integrate with the online wiki

module QuickStartGuide =

    open FSharp.Formatting.Markdown

    let SIZE = 25.0f
    let WIDTH = 1200.0f

    let doc = 
        getResourceText "QuickStart.md"
        |> Markdown.Parse

    type MarkdownUI =
        {
            Body: StaticContainer
            mutable LHeight: float32
            mutable LWidth: float32
            mutable Height: float32 // not including last line
            mutable Width: float32
        }
    module MarkdownUI =
        let addTo (parent: MarkdownUI) (child: MarkdownUI) =
            let childTotalHeight = child.Height + child.LHeight
            parent.LHeight <- max parent.LHeight childTotalHeight
            child.Body.Position <- Position.Box (0.0f, 0.0f, parent.LWidth, parent.Height, child.Width, childTotalHeight)
            parent.Body.Add child.Body
            parent.LWidth <- parent.LWidth + child.Width
            parent.Width <- max parent.LWidth parent.Width
            parent

        let newline (w: MarkdownUI) =
            w.Height <- w.Height + w.LHeight
            w.LWidth <- 0.0f
            w.LHeight <- 0.0f
            w

        let addToNL (parent: MarkdownUI) (child: MarkdownUI) = addTo parent child |> newline

        let pad (x, y) (w: MarkdownUI) =
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

    let rec span (settings: SpanSettings) (sp: MarkdownSpan) : MarkdownUI =
        match sp with
        | Literal (text, _) ->
            MarkdownUI.text text
                ((if settings.Bold then Style.color(255, 1.0f, 0.3f) else Color.White),
                    if settings.Italic then Color.Gray 
                    elif settings.HasLink then Color.Blue
                    else Color.Black)
                settings.Size
        | InlineCode (code, _) -> MarkdownUI.text code (Color.Silver, Color.Gray) settings.Size
        | Strong (body, _) -> spans { settings with Bold = true } body
        | Emphasis (body, _) -> spans { settings with Italic = true } body
        | AnchorLink (link, _) -> MarkdownUI.sym "anchorLink"
        | DirectLink (body, link, title, _) ->
            let r = spans { settings with HasLink = true } body
            r.Body.Add (Percyqaz.Flux.UI.Clickable(fun () -> openlink link))
            r
        | IndirectLink (body, link, title, _) -> MarkdownUI.sym "ilink"
        | DirectImage (body, link, title, _) -> MarkdownUI.sym "dimg"
        | IndirectImage (body, link, title, _) -> MarkdownUI.sym "iimg"
        | HardLineBreak _ -> MarkdownUI.sym "linebreak"
        | EmbedSpans _
        | LatexDisplayMath _
        | LatexInlineMath _ -> MarkdownUI.empty()

    and spans settings (sps: MarkdownSpans) : MarkdownUI =
        let block = MarkdownUI.empty()
        List.map (span settings) sps
        |> List.fold MarkdownUI.addTo block

    and addParagraph (body: MarkdownUI) (p: MarkdownParagraph) =
        match p with
        | Heading (size, body, _) -> 
            let b = MarkdownUI.empty() |> MarkdownUI.pad (0.0f, 15.0f)
            MarkdownUI.addTo b (spans { SpanSettings.Default with Size = (SIZE + 5.0f * (4.0f - float32 size)) } body)
            |> MarkdownUI.pad (0.0f, 15.0f)
        | Paragraph (body, _) -> spans SpanSettings.Default body
        | Span (body, _) -> spans SpanSettings.Default body
        | ListBlock (kind, items, _) ->
            let list = MarkdownUI.empty()
            let bullet() = 
                let b = MarkdownUI.empty() |> MarkdownUI.pad(20.0f, 0.0f)
                MarkdownUI.addTo b (MarkdownUI.text "•" (Color.White, Color.Transparent) SIZE)
                |> MarkdownUI.pad(10.0f, 0.0f)
            items
            |> List.map 
                ( 
                    fun i -> 
                        let block = bullet()
                        MarkdownUI.addTo block (i |> List.fold addParagraph (MarkdownUI.empty()))
                )
            |> List.fold MarkdownUI.addToNL list
        | HorizontalRule (char, _) -> MarkdownUI.sym "rule"
        | YamlFrontmatter _
        | TableBlock _
        | OutputBlock _
        | OtherBlock _
        | LatexBlock _
        | QuotedBlock _
        | CodeBlock _
        | EmbedParagraphs _
        | InlineHtmlBlock _ -> MarkdownUI.empty()
        |> MarkdownUI.addToNL body

    let buildMarkdownUI (doc: MarkdownDocument) =
        doc.Paragraphs
        |> List.fold addParagraph (MarkdownUI.empty())

    type MarkdownViewDialog(doc) =
        inherit Percyqaz.Flux.UI.Dialog()

        let markdown = buildMarkdownUI doc
        let flow = ScrollContainer(markdown.Body, K (markdown.Height + markdown.LHeight), Position = Position.Margin(400.0f, 100.0f))

        override this.Init(parent: Widget) =
            base.Init parent
            flow.Init this

        override this.Update(elapsedTime, moved) =
            base.Update(elapsedTime, moved)
            flow.Update(elapsedTime, moved)
            if Mouse.leftClick() || (!|"exit").Tapped() then
                this.Close()

        override this.Draw() = flow.Draw()

    let help() = (MarkdownViewDialog doc).Show()