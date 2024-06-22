namespace Interlude.Features.Wiki

open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Interlude.UI
open Interlude.UI.Menu

type WikiContent(nt) =
    inherit Container(nt)

    member val _Size = 0.0f with get, set

    interface IHeight with
        member this.Height = this._Size

type WikiBrowser() as this =
    inherit Dialog()

    let mutable flow = Unchecked.defaultof<ScrollContainer<WikiContent>>

    let buttons =
        NavigationContainer.Row(
            Position = Position.SliceTop(70.0f).Margin((Viewport.vwidth - 1400.0f) * 0.5f, 10.0f)
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
                | _ -> this.Close()
            , Position = Position.SliceLeft(150.0f)
        )
        |+ Text(
            (fun () ->
                match current_page with
                | Changelog -> sprintf "%s %s" Icons.EDIT_2 (%"wiki.changelog")
                | WikiPage p -> sprintf "%s %s  >  %s" Icons.BOOK_OPEN p.Folder p.Title
                | WikiIndex -> sprintf "%s %s" Icons.HOME (%"wiki.home")
            ),
            Align = Alignment.LEFT,
            Position = Position.Column(200.0f, 900.0f)
        )
        |+ IconButton(
            %"wiki.openinbrowser"
            , Icons.EXTERNAL_LINK
            , 50.0f
            , fun () ->
                match current_page with
                | WikiIndex -> open_url ("https://yavsrg.net/interlude/wiki/index.html")
                | WikiPage p -> open_url ("https://yavsrg.net/interlude/wiki/" + p.Filename + ".html")
                | Changelog -> open_url ("https://yavsrg.net/interlude/changelog.html")
            , Position = Position.SliceRight(300.0f)
        )

    do Heading.scroll_handler <- fun w -> flow.Scroll(w.Bounds.Top - flow.Bounds.Top)

    member private this.UpdateContent() =
        let mutable y = 0.0f
        let spacing = 35.0f
        let max_width = 1400.0f
        let con = new WikiContent(NodeType.None)

        match content with
        | Some paragraphs ->
            for paragraph in paragraphs do
                let markdown = MarkdownUI.build max_width paragraph
                markdown.Position <- Position.Box(0.0f, 0.0f, 0.0f, y, max_width, markdown.Height)
                con.Add(markdown)
                y <- y + markdown.Height + spacing

            if current_page = WikiIndex then
                let fcount = float32 Cache.index_table_of_contents.Keys.Count

                let folders =
                    FlowContainer.LeftToRight<_>(
                        (max_width - 10.0f - (fcount - 1.0f) * 20.0f) / fcount,
                        Spacing = 20.0f,
                        Position = Position.TrimTop(80.0f).Margin(5.0f, 0.0f)
                    )

                for key in Cache.index_table_of_contents.Keys do
                    let pages = Cache.index_table_of_contents.[key]

                    let links = FlowContainer.Vertical(50.0f, Position = Position.TrimTop(60.0f))

                    for p in pages do
                        links.Add(Button(p.Title, (fun () -> load_resource (WikiPage p))))

                    folders.Add(
                        FrameContainer(
                            NodeType.Container(fun () -> Some links),
                            Fill = K Colors.cyan.O2,
                            Border = K Colors.cyan_accent
                        )
                        |+ Text(key, Position = Position.SliceTop(60.0f), Color = K Colors.text_subheading)
                        |+ links
                    )

                con.Add(
                    Container(
                        NodeType.Container(fun () -> Some folders),
                        Position = Position.Box(0.0f, 0.0f, 0.0f, y, max_width, 400.0f)
                    )
                    |+ Text(
                        sprintf "%s %s" Icons.BOOK_OPEN (%"wiki.contents"),
                        Position = Position.SliceTop(70.0f).Margin(20.0f, 0.0f),
                        Align = Alignment.LEFT
                    )
                    |+ folders
                )

                y <- y + 400.0f + spacing
        | None -> con.Add(LoadingState())

        con._Size <- y - spacing + Style.PADDING * 2.0f
        flow <- ScrollContainer(con, Margin = Style.PADDING, Position = Position.CenterX(max_width).Margin(-Style.PADDING, 80.0f))

        flow.Init this

    override this.Init(parent: Widget) =
        base.Init parent
        buttons.Init this
        this.UpdateContent()

        if content.IsNone && current_page = WikiIndex then
            load_resource WikiIndex

        page_changed <- this.UpdateContent

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        buttons.Update(elapsed_ms, moved)
        flow.Update(elapsed_ms, moved)

        if Mouse.left_click () || (%%"exit").Tapped() then
            this.Close()

    override this.Draw() =
        buttons.Draw()
        flow.Draw()

    static member Show() = (WikiBrowser() :> Dialog).Show()
    
    static member ShowChangelog() = 
        if current_page <> Changelog then
            load_resource Changelog
        (WikiBrowser() :> Dialog).Show()
