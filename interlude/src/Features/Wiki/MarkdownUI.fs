namespace Interlude.Features.Wiki

open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.Data
open Interlude.UI
open Interlude.Utils
open FSharp.Formatting.Markdown

type LinkHandler =
    {
        Prefix: string
        Action: string -> unit
    }

module LinkHandler =

    let mutable private handlers : LinkHandler list = []

    let add x = handlers <- x :: handlers

    let handle (url: string) =
        let mutable handled = false

        for handler in handlers do
            if not handled && url.StartsWith(handler.Prefix, System.StringComparison.InvariantCultureIgnoreCase) then
                handler.Action url
                handled <- true

        if not handled then openUrl url

module private Span =

    let fragment (text: string, colors: Color * Color, background: Color option) =
        let t = Text(text, Color = K colors, Align = Alignment.LEFT)
        match background with
        | Some b ->
            StaticContainer(NodeType.None)
            |+ Frame(NodeType.None, Border = K b, Fill = K b, Position = Position.Margin(0.0f, 5.0f))
            |+ t
            :> Widget
        | None -> t

    let link_fragment (text: string, link: string) =
        Button(text, fun () -> LinkHandler.handle link)

    let SIZE = 22.0f
    type Settings =
        {
            Size: float32
            Strong: bool
            Emphasis: bool
            Link: string option
            InlineCode: bool
            CodeBlock: bool
            Background: Color
        }
        static member Default = { Size = SIZE; Strong = false; Emphasis = false; Link = None; CodeBlock = false; InlineCode = false; Background = Color.Transparent }

    let create_fragment (max_width: float32) (text: string) (settings: Settings) =
        let fg = 
            if settings.Strong then Style.color(255, 1.0f, 0.8f)
            else Color.White
        let bg =
            if settings.Emphasis then Color.Gray
            elif settings.InlineCode || settings.CodeBlock then Color.DarkGray
            else Color.Black
        let highlight =
            if settings.InlineCode then Some (Color.FromArgb(127, Color.Silver))
            else None

        let mutable text = text.Replace("&gt;", ">").Replace("&lt;", "<")
        let mutable remainingText = ""
        let mutable width = (Text.measure(Style.baseFont, text)) * settings.Size
        let height = settings.Size / 0.6f
        while width > max_width && text.Contains(' ') do
            let i = text.LastIndexOf(' ')
            remainingText <- text.Substring(i) + remainingText
            text <- text.Substring(0, i)
            width <- (Text.measure(Style.baseFont, text)) * settings.Size

        match settings.Link with
        | None -> (fragment (text, (fg, bg), highlight), (width, height), remainingText)
        | Some link -> (link_fragment (text, link), (width, height), remainingText)

    [<RequireQualifiedAccess>]
    type FragmentInfo =
        | Normal
        | Linebreak
        | Image of string

    let rec get_fragments (settings: Settings) (spans: MarkdownSpans) : (string * Settings * FragmentInfo) seq =
        seq {
            for sp in spans do
                match sp with
                | Literal (text, _) ->
                    if text.Contains('\n') then
                        for sp in text.Split("\n", System.StringSplitOptions.RemoveEmptyEntries) do
                            yield (sp, settings, FragmentInfo.Normal)
                            yield ("", settings, FragmentInfo.Linebreak)
                    else yield (text, settings, FragmentInfo.Normal)
                | InlineCode (code, _) -> yield (code, { settings with InlineCode = true }, FragmentInfo.Normal)
                | Strong (body, _) -> yield! get_fragments { settings with Strong = true } body
                | Emphasis (body, _) -> yield! get_fragments { settings with Emphasis = true } body
                | AnchorLink (link, _) -> ()
                | DirectLink (body, link, title, _) -> yield! get_fragments { settings with Link = Some link } body
                | IndirectLink (body, original, key, _) -> ()
                | DirectImage (body, link, title, _) -> yield (body, settings, FragmentInfo.Image link)
                | IndirectImage (body, link, key, _) -> ()
                | HardLineBreak _ -> yield ("", settings, FragmentInfo.Linebreak)
                | EmbedSpans (customSpans, _) -> ()
                | LatexDisplayMath (code, _) -> ()
                | LatexInlineMath (code, _) -> ()
        }

[<AbstractClass>]
type IParagraph() =
    inherit StaticContainer(NodeType.None)
    
    abstract member Width : float32
    abstract member Height : float32

type private Image(width, title, url) as this =
    inherit IParagraph()

    let mutable sprite : Sprite option = None

    do 
        ImageServices.get_cached_image.Request(url, fun bmp -> sync(fun () -> sprite <- Some (Sprite.upload (bmp, 1, 1, false))))
        this |* Frame(NodeType.None, Border = K (Color.FromArgb(127, 255, 255, 255)), Fill = K Color.Transparent)

    override this.Width = width
    override this.Height = width / 16.0f * 9.0f

    override this.Draw() =
        if not this.VisibleBounds.Visible then () else
        base.Draw()
        match sprite with
        | None -> Text.drawFillB(Style.baseFont, title, this.Bounds.Shrink(20.0f, 20.0f), Style.text(), Alignment.CENTER)
        | Some s -> Draw.sprite this.Bounds Color.White s

type private Spans(max_width, spans: MarkdownSpans, settings: Span.Settings) as this =
    inherit IParagraph()

    let mutable height = 0.0f

    do
        let mutable x = 0.0f
        let mutable y = 0.0f
        let mutable lineHeight = 0.0f

        let newLine() =
            x <- 0.0f
            y <- y + lineHeight
            lineHeight <- 0.0f

        for (text, settings, info) in Span.get_fragments settings spans do
            match info with
            | Span.FragmentInfo.Linebreak -> newLine()
            | Span.FragmentInfo.Image url ->
                let img = Image(max_width - x, text, url)
                lineHeight <- max lineHeight img.Height
                img.Position <- Position.Box(0.0f, 0.0f, x, y, img.Width, img.Height)
                this |* img
                newLine()
            | Span.FragmentInfo.Normal ->

            let fragment, (width, height), remaining = Span.create_fragment (max_width - x) text settings
            lineHeight <- max lineHeight height
            if width + x > max_width then newLine()
            fragment.Position <- Position.Box(0.0f, 0.0f, x, y, width, height)
            x <- x + width
            this.Add fragment

            let mutable _remaining = remaining
            while _remaining <> "" do
                newLine()
                let fragment, (width, height), remaining = Span.create_fragment (max_width - x) _remaining settings
                lineHeight <- max lineHeight height
                if width + x > max_width then newLine()
                fragment.Position <- Position.Box(0.0f, 0.0f, x, y, width, height)
                this.Add fragment
                x <- x + width
                _remaining <- remaining
        newLine()
        height <- y

    override this.Width = max_width
    override this.Height = height

    override this.Draw() = if this.VisibleBounds.Visible then base.Draw()

module private ListBlock =

    let INDENT = 45.0f
    let BULLET_SIZE = Span.SIZE / 0.6f

type private ListBlock(max_width: float32, paragraphs: IParagraph list) as this =
    inherit IParagraph()

    let mutable height = 0.0f

    do
        let mutable y = 0.0f
        for p in paragraphs do
            p.Position <- Position.Box(0.0f, 0.0f, ListBlock.INDENT, y, p.Width, p.Height)
            this
            |+ p
            |* Text("•", Position = Position.Box(0.0f, 0.0f, 0.0f, y, ListBlock.INDENT, ListBlock.BULLET_SIZE), Align = Alignment.CENTER)
            y <- y + p.Height
        height <- y

    override this.Width = max_width
    override this.Height = height
    
    override this.Draw() = if this.VisibleBounds.Visible then base.Draw()

type private Paragraphs(max_width: float32, paragraphs: IParagraph list) as this =
    inherit IParagraph()

    let SPACING = Span.SIZE

    let mutable height = 0.0f

    do
        let mutable y = 0.0f
        for p in paragraphs do
            p.Position <- Position.Box(0.0f, 0.0f, 0.0f, y, p.Width, p.Height)
            this.Add p
            y <- y + p.Height + SPACING
        height <- max 0.0f (y - SPACING)

    override this.Width = max_width
    override this.Height = height
    
    override this.Draw() = if this.VisibleBounds.Visible then base.Draw()

module Heading =
    
    let MARGIN = 10.0f

    let rec getText (body: MarkdownSpan list) =
        match body.Head with
        | Literal (text, _) -> text
        | Strong (body, _)
        | Emphasis (body, _)
        | DirectLink (body, _, _, _) -> getText body
        | _ -> ""

    let mutable scrollTo = ""
    let mutable scrollHandler : Widget -> unit = ignore

type private Heading(max_width, size, body: MarkdownSpan list) as this =
    inherit IParagraph()

    let contents = Spans(max_width - Heading.MARGIN * 2.0f, body, { Span.Settings.Default with Size = Span.SIZE + 2.0f * System.MathF.Pow(4.0f - float32 size, 2.0f) })
    let text = Heading.getText body

    do
        contents.Position <- Position.Box(0.0f, 0.0f, Heading.MARGIN, Heading.MARGIN, contents.Width, contents.Height)
        this
        |+ Frame(NodeType.None, Fill = K (Color.FromArgb(127, 0, 0, 0)), Border = K Color.Transparent)
        |* contents
    
    override this.Width = max_width
    override this.Height = contents.Height + Heading.MARGIN * 2.0f

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        if Heading.scrollTo <> "" && text.Contains(Heading.scrollTo, System.StringComparison.InvariantCultureIgnoreCase) then
            Heading.scrollTo <- ""
            Heading.scrollHandler this
            
    override this.Draw() = if this.VisibleBounds.Visible then base.Draw()

type HorizontalRule(max_width) =
    inherit IParagraph()

    override this.Width = max_width
    override this.Height = Span.SIZE

    override this.Draw() =
        if this.VisibleBounds.Visible then () else
        Draw.rect (this.Bounds.SliceTop(5.0f).Translate(0.0f, this.Bounds.Height / 2.0f - 2.5f)) (Color.FromArgb(127, 255, 255, 255))

type CodeBlock(max_width, code, language) as this =
    inherit IParagraph()

    let contents = Spans(max_width - Heading.MARGIN * 4.0f, [ Literal(code, None) ], { Span.Settings.Default with Size = Span.SIZE - 3.0f; CodeBlock = true })
    do
        contents.Position <- Position.Box(0.0f, 0.0f, Heading.MARGIN * 2.0f, Heading.MARGIN * 2.0f, contents.Width, contents.Height)
        this
        |+ Frame(NodeType.None, Fill = K (Color.FromArgb(127, 127, 127, 127)), Border = K Color.Transparent)
        |* contents
    
    override this.Width = max_width
    override this.Height = contents.Height + Heading.MARGIN * 4.0f
    
    override this.Draw() = if this.VisibleBounds.Visible then base.Draw()

module private Paragraph =

    let empty() = 
        { new IParagraph() with 
            override this.Width = 0.0f
            override this.Height = 0.0f
        }
    
    let rec create (max_width: float32) (p: MarkdownParagraph) : IParagraph =
        match p with
        | Heading (size, body, _) -> Heading(max_width, size, body)
        | Paragraph (body, _) -> Spans(max_width, body, Span.Settings.Default)
        | Span (body, _) -> Spans(max_width, body, Span.Settings.Default)
        | ListBlock (kind, items, _) -> ListBlock(max_width, List.map (createMultiple (max_width - ListBlock.INDENT)) items)
        | HorizontalRule (char, _) -> HorizontalRule(max_width)
        | CodeBlock (code, _, language, _, _) -> CodeBlock(max_width, code, language)
        | YamlFrontmatter _
        | TableBlock _ // todo
        | OutputBlock _
        | OtherBlock _
        | LatexBlock _
        | QuotedBlock _
        | EmbedParagraphs _
        | InlineHtmlBlock _ -> empty()

    and createMultiple (max_width: float32) (ps: MarkdownParagraphs) : IParagraph =
        Paragraphs(max_width, List.map (create max_width) ps)

module MarkdownUI =

    let build (max_width: float32) (doc: MarkdownDocument) = Paragraph.createMultiple max_width doc.Paragraphs