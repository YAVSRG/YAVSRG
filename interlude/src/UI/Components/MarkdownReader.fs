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
open Interlude.UI.Components

module MarkdownReader =

    open FSharp.Formatting.Markdown

    let SIZE = 25.0f
    let WIDTH = 1200.0f

    let doc = 
        getResourceText "QuickStart.md"
        |> Markdown.Parse

    type W =
        {
            Body: Widget1
            mutable LHeight: float32
            mutable LWidth: float32
            mutable Height: float32 // not including last line
            mutable Width: float32
        }
    module W =
        let addTo (parent: W) (child: W) =
            let childTotalHeight = child.Height + child.LHeight
            parent.LHeight <- max parent.LHeight childTotalHeight
            child.Body.Position ( Position.Box (0.0f, 0.0f, parent.LWidth, parent.Height, child.Width, childTotalHeight) )
            |> parent.Body.Add
            parent.LWidth <- parent.LWidth + child.Width
            parent.Width <- max parent.LWidth parent.Width
            parent

        let newline (w: W) =
            w.Height <- w.Height + w.LHeight
            w.LWidth <- 0.0f
            w.LHeight <- 0.0f
            w

        let addToNL (parent: W) (child: W) = addTo parent child |> newline

        let pad (x, y) (w: W) =
            w.LWidth <- w.LWidth + x
            w.Height <- w.Height + y
            w
            
        // constructors

        let make (w: Widget1) (width: float32, height: float32) =
            {
                Body = w
                LHeight = height
                LWidth = width
                Height = 0.0f
                Width = width
            }

        let text (str: string) (col: Color * Color) (size: float32) =
            let width = (Text.measure(Content.font, str)) * size
            make <| TextBox(K str, K col, 0.0f) <| (width, size / 0.6f)

        let sym str = text str (Color.Silver, Color.Red) 25.0f

        let empty() = make (Widget1()) (0.0f, 0.0f)


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

    let rec span (settings: SpanSettings) (sp: MarkdownSpan) : W =
        match sp with
        | Literal (text, _) ->
            W.text text
                ((if settings.Bold then Style.color(255, 1.0f, 0.3f) else Color.White),
                    if settings.Italic then Color.Gray 
                    elif settings.HasLink then Color.Blue
                    else Color.Black)
                settings.Size
        | InlineCode (code, _) -> W.text code (Color.Silver, Color.Gray) settings.Size
        | Strong (body, _) -> spans { settings with Bold = true } body
        | Emphasis (body, _) -> spans { settings with Italic = true } body
        | AnchorLink (link, _) -> W.sym "anchorLink"
        | DirectLink (body, link, title, _) ->
            let r = spans { settings with HasLink = true } body
            r.Body.Add (Clickable ((fun () -> openlink link), ignore))
            r
        | IndirectLink (body, link, title, _) -> W.sym "ilink"
        | DirectImage (body, link, title, _) -> W.sym "dimg"
        | IndirectImage (body, link, title, _) -> W.sym "iimg"
        | HardLineBreak _ -> W.sym "linebreak"
        | EmbedSpans _
        | LatexDisplayMath _
        | LatexInlineMath _ -> W.empty()

    and spans settings (sps: MarkdownSpans) : W =
        let block = W.empty()
        List.map (span settings) sps
        |> List.fold W.addTo block

    and addParagraph (body: W) (p: MarkdownParagraph) =
        match p with
        | Heading (size, body, _) -> 
            let b = W.empty() |> W.pad (0.0f, 15.0f)
            W.addTo b (spans { SpanSettings.Default with Size = (SIZE + 5.0f * (4.0f - float32 size)) } body)
            |> W.pad (0.0f, 15.0f)
        | Paragraph (body, _) -> spans SpanSettings.Default body
        | Span (body, _) -> spans SpanSettings.Default body
        | ListBlock (kind, items, _) ->
            let list = W.empty()
            let bullet() = 
                let b = W.empty() |> W.pad(20.0f, 0.0f)
                W.addTo b (W.text "•" (Color.White, Color.Transparent) SIZE)
                |> W.pad(10.0f, 0.0f)
            items
            |> List.map 
                ( 
                    fun i -> 
                        let block = bullet()
                        W.addTo block (i |> List.fold addParagraph (W.empty()))
                )
            |> List.fold W.addToNL list
        | HorizontalRule (char, _) -> W.sym "rule"
        | YamlFrontmatter _
        | TableBlock _
        | OutputBlock _
        | OtherBlock _
        | LatexBlock _
        | QuotedBlock _
        | CodeBlock _
        | EmbedParagraphs _
        | InlineHtmlBlock _ -> W.empty()
        |> W.addToNL body

    let buildDocWidget (doc: MarkdownDocument) =
        doc.Paragraphs
        |> List.fold addParagraph (W.empty())
        |> fun wb -> wb.Body.Position { Position.Margin 10.0f with Bottom = 0.0f %+ (wb.Height + wb.LHeight) }

    type MarkdownViewDialog(doc) as this =
        inherit Dialog()

        let frame =
            Frame((fun () -> Style.color(200, 0.1f, 0.0f)), (fun () -> Style.color(255, 1.0f, 0.0f)))
            |-+ (FlowContainer() |-+ (buildDocWidget doc)).Position( Position.Margin(15.0f, 0.0f) )

        do
            frame.Position 
                { 
                    Left = 0.5f %+ (-WIDTH * 0.5f)
                    Top = 0.0f %+ Viewport.vheight
                    Right = 0.5f %+ (WIDTH * 0.5f)
                    Bottom = 1.0f %+ (Viewport.vheight - 200.0f)
                }
            |> this.Add
            frame.Move(-WIDTH * 0.5f, 100.0f, WIDTH * 0.5f, -100.0f)

        override this.Update(elapsedTime, bounds) =
            base.Update(elapsedTime, bounds)
            if Mouse.leftClick() || (!|"exit").Tapped() then
                this.BeginClose()
                frame.Move(-WIDTH * 0.5f, Viewport.vheight, WIDTH * 0.5f, Viewport.vheight - 200.0f)
        override this.OnClose() = ()

    let help() = Dialog.add (MarkdownViewDialog doc)