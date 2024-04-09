namespace Interlude.Features.Wiki

open Percyqaz.Common
open Percyqaz.Data
open Percyqaz.Data.Markdown
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Data
open Interlude.Utils
open Interlude.UI

module Wiki =

    [<Json.AutoCodec>]
    type WikiPage =
        {
            Folder: string
            Title: string
            Filename: string
        }

    type Resource =
        | WikiIndex
        | WikiPage of WikiPage
        | Changelog

    module Cache =

        let mutable changelog_content: MarkdownDocument array option = None
        let mutable index_content: MarkdownDocument array option = None
        let mutable index_table_of_contents: Map<string, WikiPage list> = Map.empty

        let mutable page_cache_by_filename: Map<string, MarkdownDocument array option> =
            Map.empty

        let mutable pages_by_filename: Map<string, WikiPage> = Map.empty

    let mutable private loading = false
    let mutable private page_history = [ WikiIndex ]
    let mutable private current_page = WikiIndex
    let mutable private content: MarkdownDocument array option = None
    let mutable private page_changed: unit -> unit = ignore

    let private page_loader =
        { new Async.Service<Resource, MarkdownDocument array>() with
            override this.Handle(req) =
                async {
                    match req with
                    | WikiIndex ->
                        if Cache.index_content.IsNone then
                            match!
                                WebServices.download_string.RequestAsync(
                                    sprintf "https://raw.%s.com/YAVSRG/YAVSRG/main/interlude/docs/wiki/index.md" "githubusercontent"
                                )
                            with
                            | Some md ->
                                match!
                                    WebServices.download_json_async (
                                        sprintf "https://raw.%s.com/YAVSRG/YAVSRG/main/interlude/docs/wiki/index.json" "githubusercontent"
                                    )
                                with
                                | Some toc ->
                                    Cache.index_table_of_contents <- toc

                                    Cache.page_cache_by_filename <-
                                        toc
                                        |> Map.toSeq
                                        |> Seq.map snd
                                        |> Seq.concat
                                        |> Seq.map (fun p -> p.Filename, None)
                                        |> Map.ofSeq

                                    Cache.pages_by_filename <-
                                        toc
                                        |> Map.toSeq
                                        |> Seq.map snd
                                        |> Seq.concat
                                        |> Seq.map (fun p -> p.Filename, p)
                                        |> Map.ofSeq

                                    Cache.index_content <- Some [| Markdown.Parse md |]
                                | None -> ()
                            | None -> ()

                        return Cache.index_content |> Option.defaultValue [||]

                    | WikiPage p ->
                        if Cache.page_cache_by_filename.[p.Filename].IsNone then
                            match!
                                WebServices.download_string.RequestAsync(
                                    sprintf "https://raw.%s.com/YAVSRG/YAVSRG/main/interlude/docs/wiki/%s.md" "githubusercontent" p.Filename
                                )
                            with
                            | Some md ->
                                let sections =
                                    md
                                        .Split("---", 3, System.StringSplitOptions.TrimEntries)
                                        .[2].Split("::::", System.StringSplitOptions.TrimEntries)

                                Cache.page_cache_by_filename <-
                                    Cache.page_cache_by_filename.Add(
                                        p.Filename,
                                        Some(sections |> Array.map Markdown.Parse)
                                    )
                            | None -> ()

                        return Cache.page_cache_by_filename.[p.Filename] |> Option.defaultValue [||]

                    | Changelog ->
                        if Cache.changelog_content.IsNone then
                            match!
                                WebServices.download_string.RequestAsync(
                                    sprintf "https://raw.%s.com/YAVSRG/YAVSRG/main/interlude/docs/changelog.md" "githubusercontent"
                                )
                            with
                            | Some md -> Cache.changelog_content <- Some [| Markdown.Parse md |]
                            | None -> ()

                        return Cache.changelog_content |> Option.defaultValue [||]
                }
        }

    // todo: reimplement links to specific headers
    //if page.Contains('#') then
    //Heading.scrollTo <- page.Substring(page.LastIndexOf '#' + 1).Replace('-', ' ')
    //page.Substring(0, page.LastIndexOf '#')

    let load_resource (res) =
        if not loading then
            loading <- true
            page_history <- res :: (List.except [ res ] page_history)
            current_page <- res

            page_loader.Request(
                res,
                fun md ->
                    content <- Some md
                    loading <- false
                    sync page_changed
            )

    do
        LinkHandler.add
            {
                Condition =
                    fun url ->
                        Cache.pages_by_filename.ContainsKey(
                            url.Replace(".html", "", System.StringComparison.InvariantCultureIgnoreCase)
                        )
                Action =
                    fun url ->
                        Cache.pages_by_filename.[url.Replace(
                            ".html",
                            "",
                            System.StringComparison.InvariantCultureIgnoreCase
                        )]
                        |> WikiPage
                        |> load_resource
            }

    type WikiContent(nt) =
        inherit Container(nt)

        member val _Size = 0.0f with get, set

        interface DynamicSize with
            member this.Size = this._Size

            member this.OnSizeChanged
                with set _ = ()

    type Browser() as this =
        inherit Dialog()

        let mutable flow = Unchecked.defaultof<ScrollContainer<WikiContent>>

        let buttons =
            NavigationContainer.Row<Widget>(
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

            con._Size <- y - spacing
            flow <- ScrollContainer(con, Position = Position.Margin((Viewport.vwidth - 1400.0f) * 0.5f, 80.0f))

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

    let show () = (Browser()).Show()

    let show_changelog () =
        if current_page <> Changelog then
            load_resource Changelog

        (Browser()).Show()
