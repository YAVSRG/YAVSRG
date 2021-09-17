namespace Interlude.UI

open System.Drawing
open System.Diagnostics
open OpenTK.Windowing.GraphicsLibraryFramework
open Interlude
open Interlude.Utils
open Interlude.Input
open Interlude.Graphics
open Interlude.UI.Components

module MarkdownReader =
    open System.IO
    open FSharp.Formatting.Markdown

    let SIZE = 25.0f
    let WIDTH = 1200.0f

    let doc = 
        use s = Utils.getResourceStream("QuickStart.md")
        use r = new StreamReader(s)
        r.ReadToEnd()
        |> Markdown.Parse

    type WBuilder = {
        body: Widget
        lastLineHeight: float32
        height: float32
        width: float32
        right: float32
    }
    module WBuilder =
        let add (x: WBuilder) (child: WBuilder) =
            let cheight = child.height + child.lastLineHeight
            if 
                x.right + child.width > x.width
            then //new line
                child.body
                |> positionWidget(0.0f, 0.0f, x.height + x.lastLineHeight, 0.0f, child.width, 0.0f, x.height + x.lastLineHeight + cheight, 0.0f)
                |> x.body.Add
                { x with height = x.height + x.lastLineHeight; lastLineHeight = cheight; right = child.width }
            else //not new line
                child.body
                |> positionWidget(x.right, 0.0f, x.height, 0.0f, x.right + child.width, 0.0f, x.height + cheight, 0.0f)
                |> x.body.Add
                { x with lastLineHeight = max x.lastLineHeight cheight; right = x.right + child.width }

        let text (str: string) (col: Color * Color) (size: float32) : WBuilder =
            //let str = str + " "
            {
                body = TextBox(K str, K col, 0.0f)
                lastLineHeight = 0.0f
                height = size / 0.6f
                width = (Text.measure(Themes.font(), str) - 0.75f) * size
                right = 0.0f
            }

        let sym str = text str (Color.Silver, Color.Red) 25.0f

        let frame (width: float32) : WBuilder =
            {
                body = new Widget()
                lastLineHeight = 0.0f
                height = 0.0f
                width = width
                right = 0.0f
            }

        let dummy() = frame 0.0f

        let pack (alwaysNewline: bool) (xs: WBuilder list) : WBuilder =
            let width =
                if alwaysNewline then WIDTH
                else let widest = xs |> List.maxBy (fun i -> i.width) in widest.width
            List.fold add (frame width) xs

    type SpanSettings = {
        Size: float32
        Bold: bool
        Italic: bool
        HasLink: bool
    }
    with
        static member Default = { Size = SIZE; Bold = false; Italic = false; HasLink = false }

    let openlink (str: string) =
        try Process.Start (ProcessStartInfo (str, UseShellExecute=true)) |> ignore
        with err -> Prelude.Common.Logging.Debug ("Failed to open link: " + str, err)

    let rec formatSpan (settings: SpanSettings) (sp: MarkdownSpan) : WBuilder =
        match sp with
        | Literal (text, _) ->
            WBuilder.text text
                (Color.White, 
                    if settings.Bold then Color.Black
                    elif settings.Italic then Color.Gray 
                    elif settings.HasLink then Color.Blue
                    else Color.Transparent)
                settings.Size
        | InlineCode (code, _) -> WBuilder.text code (Color.Silver, Color.Transparent) settings.Size
        | Strong (body, _) -> spans { settings with Bold = true } false body
        | Emphasis (body, _) -> spans { settings with Italic = true } false body
        | AnchorLink (link, _) -> WBuilder.sym "anchorLink"
        | DirectLink (body, link, title, _) ->
            let r = spans { settings with HasLink = true } false body
            r.body.Add (Clickable ((fun () -> openlink link), ignore))
            r
        | IndirectLink (body, link, title, _) -> WBuilder.sym "ilink"
        | DirectImage (body, link, title, _) -> WBuilder.sym "dimg"
        | IndirectImage (body, link, title, _) -> WBuilder.sym "iimg"
        | HardLineBreak _ -> WBuilder.sym "linebreak"
        | EmbedSpans _
        | LatexDisplayMath _
        | LatexInlineMath _ -> WBuilder.dummy()

    and spans settings alwaysNewline (sps: MarkdownSpans) : WBuilder =
        sps
        |> List.map (formatSpan settings)
        |> WBuilder.pack alwaysNewline

    and formatParagraph (width: float32) (p: MarkdownParagraph) =
        match p with
        | Heading (size, body, _) -> spans { SpanSettings.Default with Size = (SIZE + 8.0f * float32 size) } true body
        | Paragraph (body, _) -> spans SpanSettings.Default true body
        | Span (body, _) -> spans SpanSettings.Default true body
        | ListBlock (kind, items, _) ->
            let bullet() = WBuilder.text "  •" (Color.White, Color.Transparent) SIZE
            items
            |> List.map (fun i -> i |> List.map (formatParagraph width) |> List.fold WBuilder.add (WBuilder.frame (width - bullet().width)))
            |> List.fold (fun x w -> WBuilder.add (WBuilder.add x (bullet())) w) (WBuilder.frame width)
        | HorizontalRule (char, _) -> WBuilder.sym "rule"
        | YamlFrontmatter _
        | TableBlock _
        | OutputBlock _
        | OtherBlock _
        | LatexBlock _
        | QuotedBlock _
        | CodeBlock _
        | EmbedParagraphs _
        | InlineHtmlBlock _ -> WBuilder.dummy()

    let buildDocWidget (doc: MarkdownDocument) =
        doc.Paragraphs
        |> List.map (formatParagraph 1500.0f)
        |> List.fold WBuilder.add (WBuilder.frame 1500.0f)
        |> fun wb -> wb.body |> positionWidget(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, wb.height + wb.lastLineHeight, 0.0f)

    type MarkdownViewDialog(doc) as this =
        inherit Dialog()

        let fc = FlowContainer()
        let frame =
            let f = Frame((fun () -> Globals.accentShade(200, 0.7f, 0.0f)), (fun () -> Globals.accentShade(255, 1.0f, 0.3f)))
            doc |> buildDocWidget |> fc.Add
            fc |> f.Add
            f

        do
            frame
            |> positionWidget(-WIDTH * 0.5f, 0.5f, Render.vheight, 0.0f, WIDTH * 0.5f, 0.5f, Render.vheight - 200.0f, 1.0f)
            |> this.Add
            frame.Move(-WIDTH * 0.5f, 100.0f, WIDTH * 0.5f, -100.0f)

        override this.Update(elapsedTime, bounds) =
            base.Update(elapsedTime, bounds)
            if Mouse.Click MouseButton.Left || Options.options.Hotkeys.Exit.Value.Tapped() then
                this.BeginClose()
                frame.Move(-WIDTH * 0.5f, Render.vheight, WIDTH * 0.5f, Render.vheight - 200.0f)
        override this.OnClose() = ()

    let help() = Globals.addDialog (MarkdownViewDialog doc)