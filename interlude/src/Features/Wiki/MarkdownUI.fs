namespace Interlude.Features.Wiki

open Percyqaz.Common
open Percyqaz.Data.Markdown
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Percyqaz.Flux.Windowing
open Prelude
open Prelude.Data
open Interlude.UI

type LinkHandler =
    {
        Condition: string -> bool
        Action: string -> unit
    }

module LinkHandler =

    let mutable private handlers: LinkHandler list = []

    let add x = handlers <- x :: handlers

    let handle (url: string) =
        let mutable handled = false

        for handler in handlers do
            if not handled && handler.Condition url then
                handler.Action url
                handled <- true

        if not handled then
            open_url url

type private SpanRenderingContext =
    {
        LineLength: float32
        mutable X: float32
        mutable Y: float32
        mutable CurrentLineHeight: float32
    }
    member this.RemainingSpaceOnLine = this.LineLength - this.X
    member this.Advance(width, height) : bool =
        if width <= this.RemainingSpaceOnLine then
            this.X <- this.X + width
            this.CurrentLineHeight <- max this.CurrentLineHeight height
            true
        else
            false
    member this.StartNewLine() =
        this.X <- 0.0f
        this.Y <- this.Y + this.CurrentLineHeight
        this.CurrentLineHeight <- 0.0f

module private Span =

    let fragment (text: string, colors: Color * Color, background: Color option) =
        let t =
            Text(text)
                .Color(colors)
                .Align(Alignment.LEFT)

        match background with
        | Some b ->
            Container(NodeType.None)
            |+ Frame(Border = K b, Fill = K b)
                .Position(Position.ShrinkY(5.0f))
            |+ t
            :> Widget
        | None -> t

    let link_fragment (text: string, link: string) =
        { new Button(text, (fun () -> LinkHandler.handle link)) with
            override this.Draw() =
                Render.rect (this.Bounds.SliceB(2.0f).Translate(2.0f, 2.0f)) Colors.shadow_2
                Render.rect (this.Bounds.SliceB(2.0f)) (if this.Focused then Colors.yellow_accent else Colors.white)
                base.Draw()
        }

    let SIZE = 25.0f

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
        static member Default =
            {
                Size = SIZE
                Strong = false
                Emphasis = false
                Link = None
                CodeBlock = false
                InlineCode = false
                Background = Color.Transparent
            }

    let create_fragment (ctx: SpanRenderingContext) (text: string) (settings: Settings) =
        let fg =
            if settings.Strong then
                Colors.green_accent
            elif settings.Emphasis then
                Colors.red_accent
            elif settings.InlineCode || settings.CodeBlock then
                Colors.grey_1
            else
                Colors.white

        let bg = Colors.black

        let highlight =
            if settings.InlineCode then
                Some(Colors.shadow_2.O2)
            else
                None

        let mutable text =
            text.Replace("&gt;", ">").Replace("&lt;", "<").Replace("&amp;", "&")

        let mutable remaining_text = ""
        let mutable width = (Text.measure (Style.font, text)) * settings.Size
        let height = settings.Size / 0.6f

        while width > ctx.RemainingSpaceOnLine && text.Contains(' ') do
            let i = text.LastIndexOf(' ')
            remaining_text <- text.Substring(i) + remaining_text
            text <- text.Substring(0, i)
            width <- (Text.measure (Style.font, text)) * settings.Size

        match settings.Link with
        | None -> (fragment (text, (fg, bg), highlight), (width, height), remaining_text)
        | Some link -> (link_fragment (text, link), (width, height), remaining_text)

    [<RequireQualifiedAccess>]
    type FragmentInfo =
        | Normal
        | Linebreak
        | Image of string

    let rec get_fragments (settings: Settings) (spans: MarkdownSpans) : (string * Settings * FragmentInfo) seq =
        seq {
            for sp in spans do
                match sp with
                | Literal(text, _) ->
                    if text.Contains('\n') then
                        for sp in text.Split("\n", System.StringSplitOptions.RemoveEmptyEntries) do
                            yield (sp, settings, FragmentInfo.Normal)
                            yield ("", settings, FragmentInfo.Linebreak)
                    else
                        yield (text, settings, FragmentInfo.Normal)
                | InlineCode(code, _) -> yield (code, { settings with InlineCode = true }, FragmentInfo.Normal)
                | Strong(body, _) -> yield! get_fragments { settings with Strong = true } body
                | Emphasis(body, _) -> yield! get_fragments { settings with Emphasis = true } body
                | AnchorLink(link, _) -> ()
                | DirectLink(body, link, title, _) -> yield! get_fragments { settings with Link = Some link } body
                | IndirectLink(body, original, key, _) -> ()
                | DirectImage(body, link, title, _) -> yield (body, settings, FragmentInfo.Image link)
                | IndirectImage(body, link, key, _) -> ()
                | HardLineBreak _ -> yield ("", settings, FragmentInfo.Linebreak)
                | EmbedSpans(customSpans, _) -> ()
                | LatexDisplayMath(code, _) -> ()
                | LatexInlineMath(code, _) -> ()
        }

[<AbstractClass>]
type IParagraph() =
    inherit Container(NodeType.None)

    abstract member Width: float32
    abstract member Height: float32

    interface IHeight with
        override this.Height = this.Height

    interface IWidth with
        override this.Width = this.Width

type private Image(width, title, url) as this =
    inherit IParagraph()

    let mutable sprite: Sprite option = None
    let fade = Animation.Fade 0.0f

    do
        ImageServices.get_cached_image.Request(
            url,
            function
            | Some bmp ->
                GameThread.defer (fun () ->
                    sprite <- Some(Sprite.upload_one false LinearSampling (SpriteUpload.OfImage("WIKI_IMAGE", bmp)))
                    fade.Target <- 1.0f
                )
            | None -> Logging.Warn "Failed to load wiki image '%s'" url
        )

        this |+ LoadingIndicator.Border(fun () -> sprite.IsNone)
        |* Frame(Border = K(Color.FromArgb(127, 255, 255, 255)), Fill = K Color.Transparent)

    override this.Width = width
    override this.Height = width / 16.0f * 9.0f

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        fade.Update elapsed_ms

    override this.Draw() =
        if not this.VisibleBounds.Visible then
            ()
        else
            base.Draw()

            match sprite with
            | Some s -> Render.sprite this.Bounds (Color.White.O4a fade.Alpha) s
            | None -> ()

type private Spans(max_width, spans: MarkdownSpans, settings: Span.Settings) as this =
    inherit IParagraph()

    let MARGIN = 15.0f
    let ctx =
        {
            LineLength = max_width - MARGIN * 2.0f
            X = 0.0f
            Y = 0.0f
            CurrentLineHeight = 0.0f
        }
    let mutable height = 0.0f

    do
        for (text, settings, info) in Span.get_fragments settings spans do
            match info with
            | Span.FragmentInfo.Linebreak -> ctx.Advance(0.0f, Span.SIZE) |> ignore; ctx.StartNewLine()
            | Span.FragmentInfo.Image url ->
                let img = Image(ctx.RemainingSpaceOnLine, text, url)
                img.Position <- Position.Box(0.0f, 0.0f, ctx.X, ctx.Y, img.Width, img.Height)
                this |* img
                ctx.Advance(img.Width, img.Height) |> ignore
                ctx.StartNewLine()
            | Span.FragmentInfo.Normal ->

            let mutable _remaining = text

            while _remaining <> "" do
                let fragment, (width, height), remaining =
                    Span.create_fragment ctx _remaining settings
                if ctx.Advance(width, height) then
                    fragment.Position <- Position.Box(0.0f, 0.0f, MARGIN + ctx.X - width, ctx.Y, width, height)
                else
                    ctx.StartNewLine()
                    fragment.Position <- Position.Box(0.0f, 0.0f, MARGIN + ctx.X, ctx.Y, width, height)
                this.Add fragment
                _remaining <- remaining

        ctx.StartNewLine()
        height <- ctx.Y

    override this.Width = max_width
    override this.Height = height

    override this.Draw() =
        if this.VisibleBounds.Visible then
            base.Draw()

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

            this |+ p
            |* Text("•")
                .Align(Alignment.CENTER)
                .Position(Position.Box(0.0f, 0.0f, 0.0f, y, ListBlock.INDENT, ListBlock.BULLET_SIZE))

            y <- y + p.Height

        height <- y

    override this.Width = max_width
    override this.Height = height

    override this.Draw() =
        if this.VisibleBounds.Visible then
            base.Draw()

type private Paragraphs(nested: bool, max_width: float32, paragraphs: IParagraph list) as this =
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

        if not nested then
            height <- height + 10.0f

    override this.Width = max_width
    override this.Height = height

    override this.Draw() =
        if this.VisibleBounds.Visible then
            if not nested then
                Render.border Style.PADDING this.Bounds Colors.cyan_accent
                Render.rect this.Bounds Colors.cyan.O2

            base.Draw()

module Heading =

    let MARGIN_X = 10.0f
    let MARGIN_Y = 10.0f

    let rec extract_text (body: MarkdownSpan list) =
        match body.Head with
        | Literal(text, _) -> text
        | Strong(body, _)
        | Emphasis(body, _)
        | DirectLink(body, _, _, _) -> extract_text body
        | _ -> ""

    let mutable scroll_to = ""
    let mutable scroll_handler: Widget -> unit = ignore

type private Heading(max_width, size, body: MarkdownSpan list) as this =
    inherit IParagraph()

    let contents =
        Spans(
            max_width - Heading.MARGIN_X * 2.0f,
            body,
            { Span.Settings.Default with
                Size = Span.SIZE + 1.0f * System.MathF.Pow(4.0f - float32 size, 2.0f)
            }
        )

    let text = Heading.extract_text body

    do
        contents.Position <-
            Position.Box(0.0f, 0.0f, Heading.MARGIN_X, Heading.MARGIN_Y, contents.Width, contents.Height)

        this |* contents

    override this.Width = max_width
    override this.Height = contents.Height + Heading.MARGIN_Y * 2.0f

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if
            Heading.scroll_to <> ""
            && text.Contains(Heading.scroll_to, System.StringComparison.InvariantCultureIgnoreCase)
        then
            Heading.scroll_to <- ""
            Heading.scroll_handler this

    override this.Draw() =
        if this.VisibleBounds.Visible then
            Render.rect (this.Bounds.Shrink(Style.PADDING * 2.0f)) Colors.black.O2
            base.Draw()

type HorizontalRule(max_width) =
    inherit IParagraph()

    override this.Width = max_width
    override this.Height = Span.SIZE

    override this.Draw() =
        if this.VisibleBounds.Visible then
            Render.rect (this.Bounds.SliceT(5.0f).Translate(0.0f, this.Bounds.Height / 2.0f - 2.5f)) Colors.white.O2

type CodeBlock(max_width, code, language) as this =
    inherit IParagraph()

    let contents =
        Spans(
            max_width - Heading.MARGIN_X * 4.0f,
            [ Literal(code, None) ],
            { Span.Settings.Default with
                Size = Span.SIZE - 3.0f
                CodeBlock = true
            }
        )

    do
        contents.Position <-
            Position.Box(0.0f, 0.0f, Heading.MARGIN_X * 2.0f, Heading.MARGIN_Y * 2.0f, contents.Width, contents.Height)

        this |+ Frame(Fill = K Colors.shadow_2.O2, Border = K Color.Transparent)
        |* contents

    override this.Width = max_width
    override this.Height = contents.Height + Heading.MARGIN_Y * 4.0f

    override this.Draw() =
        if this.VisibleBounds.Visible then
            base.Draw()

module private Paragraph =

    let empty () =
        { new IParagraph() with
            override this.Width = 0.0f
            override this.Height = 0.0f
        }

    let rec create (max_width: float32) (p: MarkdownParagraph) : IParagraph =
        match p with
        | Heading(size, body, _) -> Heading(max_width, size, body)
        | Paragraph(body, _) -> Spans(max_width, body, Span.Settings.Default)
        | Span(body, _) -> Spans(max_width, body, Span.Settings.Default)
        | ListBlock(kind, items, _) ->
            ListBlock(max_width, List.map (create_many true (max_width - ListBlock.INDENT)) items)
        | HorizontalRule(char, _) -> HorizontalRule(max_width)
        | CodeBlock(code, _, language, _, _, _) -> CodeBlock(max_width, code, language)
        | YamlFrontmatter _
        | TableBlock _ // todo
        | OutputBlock _
        | OtherBlock _
        | LatexBlock _
        | QuotedBlock _
        | EmbedParagraphs _
        | InlineHtmlBlock _ -> empty ()

    and create_many (nested: bool) (max_width: float32) (ps: MarkdownParagraphs) : IParagraph =
        Paragraphs(nested, max_width, List.map (create max_width) ps)

module MarkdownUI =

    let build (max_width: float32) (doc: MarkdownDocument) =
        Paragraph.create_many false max_width doc.Paragraphs