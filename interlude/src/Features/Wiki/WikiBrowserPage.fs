namespace Interlude.Features.Wiki

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Interlude.UI

type WikiContent(nt) =
    inherit Container(nt)

    member val _Size = 0.0f with get, set

    interface IHeight with
        member this.Height = this._Size

type WikiBrowserPage() =
    inherit Page()

    let container = SwapContainer(Dummy(NodeType.Leaf))

    let buttons =
        NavigationContainer.Row(
            Position = Position.SliceT(70.0f).SliceX(1400.0f).Shrink(0.0f, 10.0f)
        )
        |+ Button(
            fun () ->
                if page_history.Length < 2 then
                    Icons.X + " " + %"wiki.close"
                else
                    Icons.ARROW_LEFT_CIRCLE + " " + %"menu.back"
            , fun () ->
                match page_history with
                | x :: y :: xs ->
                    load_resource y
                    page_history <- y :: xs
                | _ -> Menu.Back()
        )
            .Position(Position.SliceL(150.0f))
        |+ Text(fun () ->
            match current_page with
            | Changelog -> sprintf "%s %s" Icons.EDIT_2 (%"wiki.changelog")
            | WikiPage p -> sprintf "%s %s  >  %s" Icons.BOOK_OPEN p.Folder p.Title
            | WikiIndex -> sprintf "%s %s" Icons.HOME (%"wiki.home")
        )
            .Align(Alignment.LEFT)
            .Position(Position.SliceL(200.0f, 900.0f))
        |+ Button(Icons.EXTERNAL_LINK + " " + %"wiki.openinbrowser", fun () ->
            match current_page with
            | WikiIndex -> open_url ("https://yavsrg.net/interlude/wiki/index.html")
            | WikiPage p -> open_url ("https://yavsrg.net/interlude/wiki/" + p.Filename + ".html")
            | Changelog -> open_url ("https://yavsrg.net/interlude/changelog.html")
        )
            .Position(Position.SliceR(300.0f))

    override this.Header() = buttons

    member private this.UpdateContent() =
        let mutable y = 0.0f
        let PARAGRAPH_SPACING = 35.0f
        let PAGE_WIDTH = 1400.0f
        let content = new WikiContent(NodeType.Leaf)

        match loaded_content with
        | Some paragraphs ->
            for paragraph in paragraphs do
                let markdown = MarkdownUI.build PAGE_WIDTH paragraph
                markdown.Position <- Position.Box(0.0f, 0.0f, 0.0f, y, PAGE_WIDTH, markdown.Height)
                content.Add(markdown)
                y <- y + markdown.Height + PARAGRAPH_SPACING

            if current_page = WikiIndex then
                let fcount = float32 Cache.index_table_of_contents.Keys.Count

                let folders =
                    FlowContainer.LeftToRight<_>((PAGE_WIDTH - 10.0f - (fcount - 1.0f) * 20.0f) / fcount)
                        .Spacing(20.0f)
                        .Position(Position.ShrinkT(80.0f).ShrinkX(5.0f))

                for key in Cache.index_table_of_contents.Keys do
                    let pages = Cache.index_table_of_contents.[key]

                    let links =
                        FlowContainer.Vertical(50.0f)
                            .Position(Position.ShrinkT(60.0f))

                    for p in pages do
                        links.Add(Button(p.Title, (fun () -> load_resource (WikiPage p))))

                    folders.Add(
                        FrameContainer(
                            NodeType.Container(fun () -> Some links),
                            Fill = K Colors.cyan.O2,
                            Border = K Colors.cyan_accent
                        )
                        |+ Text(key)
                            .Color(Colors.text_subheading)
                            .Position(Position.SliceT(60.0f))
                        |+ links
                    )

                content.Add(
                    Container(NodeType.Container(fun () -> Some folders))
                        .Position(Position.Box(0.0f, 0.0f, 0.0f, y, PAGE_WIDTH, 400.0f))
                    |+ Text(sprintf "%s %s" Icons.BOOK_OPEN (%"wiki.contents"))
                        .Align(Alignment.LEFT)
                        .Position(Position.SliceT(70.0f).Shrink(20.0f, 0.0f))
                    |+ folders
                )

                y <- y + 400.0f + PARAGRAPH_SPACING
        | None -> content.Add(LoadingState())

        content._Size <- y - PARAGRAPH_SPACING + Style.PADDING * 2.0f
        let scroll_container =
            ScrollContainer(content)
                .Margin(Style.PADDING)
                .Position(Position.SliceX(PAGE_WIDTH).Shrink(-Style.PADDING, 80.0f))
        Heading.scroll_handler <- fun w -> scroll_container.Scroll(w.Bounds.Top - scroll_container.Bounds.Top)
        container.Current <- scroll_container

    override this.Content() =
        this.UpdateContent()

        if loaded_content.IsNone && current_page = WikiIndex then
            load_resource WikiIndex

        page_changed <- this.UpdateContent

        container

    override this.Footer() = Dummy()

    override this.OnClose() = ()
    override this.Title = %"menu.wiki"

    static member Show() =
        (WikiBrowserPage() :> Page).Show()

    static member ShowChangelog() =
        load_resource Changelog
        (WikiBrowserPage() :> Page).Show()