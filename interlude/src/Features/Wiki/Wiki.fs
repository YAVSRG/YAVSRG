namespace Interlude.Features.Wiki

open Percyqaz.Common
open Percyqaz.Json
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude.Data
open Interlude.Utils
open Interlude.UI
open FSharp.Formatting.Markdown

module Wiki =

    [<Json.AutoCodec>]
    type WikiPage = { Folder: string; Title: string; Filename: string }

    type Resource =
        | WikiIndex
        | WikiPage of WikiPage
        | Changelog


    module Cache =
        
        let mutable changelog_content: MarkdownDocument array option = None
        let mutable index_content: MarkdownDocument array option = None
        let mutable index_table_of_contents: Map<string, WikiPage list> = Map.empty
        let mutable page_cache_by_filename: Map<string, MarkdownDocument array option> = Map.empty

    let mutable private page_history = []
    let mutable private current_page = WikiIndex
    let mutable private content : MarkdownDocument array option = None
    let mutable private page_changed : unit -> unit = ignore

    let private page_loader =
        { new Async.Service<Resource, MarkdownDocument array>() with
            override this.Handle(req) =
                async {
                    match req with
                    | WikiIndex ->
                        if Cache.index_content.IsNone then
                            match! WebServices.download_string.RequestAsync("https://raw.githubusercontent.com/YAVSRG/Interlude/main/docs/wiki/index.md") with 
                            | Some md ->
                                match! WebServices.download_json_async("https://raw.githubusercontent.com/YAVSRG/Interlude/main/docs/wiki/index.json") with
                                | Some toc -> 
                                    Cache.index_table_of_contents <- toc
                                    Cache.page_cache_by_filename <- toc |> Map.toSeq |> Seq.map snd |> Seq.concat |> Seq.map (fun p -> p.Filename, None) |> Map.ofSeq
                                    Cache.index_content <- Some [|Markdown.Parse md|]
                                | None -> () // log error
                            | None -> () // log error
                        return Cache.index_content |> Option.defaultValue [||]

                    | WikiPage p ->
                        if Cache.page_cache_by_filename.[p.Filename].IsNone then
                            match! WebServices.download_string.RequestAsync("https://raw.githubusercontent.com/YAVSRG/Interlude/main/docs/wiki/" + p.Filename + ".md") with
                            | Some md ->
                                let sections = md.Split("---", 3, System.StringSplitOptions.TrimEntries).[2].Split("::::", System.StringSplitOptions.TrimEntries)
                                Cache.page_cache_by_filename <- Cache.page_cache_by_filename.Add(p.Filename, Some (sections |> Array.map Markdown.Parse))
                            | None -> () // log error
                        return Cache.page_cache_by_filename.[p.Filename] |> Option.defaultValue [||]

                    | Changelog ->
                        if Cache.changelog_content.IsNone then
                            match! WebServices.download_string.RequestAsync("https://raw.githubusercontent.com/YAVSRG/Interlude/main/docs/changelog.md") with
                            | Some md -> 
                                Cache.changelog_content <- Some [|Markdown.Parse md|]
                            | None -> () // log error
                        return Cache.changelog_content |> Option.defaultValue [||]
                }       

        }

            //if page.Contains('#') then
                //Heading.scrollTo <- page.Substring(page.LastIndexOf '#' + 1).Replace('-', ' ')
                //page.Substring(0, page.LastIndexOf '#')

    let load_resource (res) =
        page_history <- res :: (List.except [res] page_history)
        current_page <- res
        page_loader.Request(res, fun md -> content <- Some md; sync page_changed)

    do
        //LinkHandler.add { 
        //    Prefix = "https://github.com/YAVSRG/Interlude/wiki/"
        //    Action = fun s -> s.Replace("https://github.com/YAVSRG/Interlude/wiki/", "") |> load_wiki_page
        //}
        load_resource WikiIndex

    type Browser() as this =
        inherit Dialog()

        let mutable flow = Unchecked.defaultof<ScrollContainer>

        let buttons = 
            NavigationContainer.Row<Widget>(Position = Position.SliceTop(70.0f).Margin((Viewport.vwidth - 1400.0f) * 0.5f, 10.0f))
            |+ IconButton(L"menu.back", Icons.back, 50.0f,
                fun () ->
                    match page_history with
                    | x :: y :: xs -> 
                        load_resource y
                        page_history <- y :: xs
                    | _ -> this.Close()
                ,
                Disabled = (fun () -> page_history.Length < 2),
                Position = Position.SliceLeft(150.0f))
            |+ Text(
                (fun () ->
                    match current_page with
                    | Changelog -> Icons.edit + " Changelog"
                    | WikiPage p -> sprintf "%s %s  >  %s" Icons.wiki2 p.Folder p.Title
                    | WikiIndex -> sprintf "%s Home" Icons.wiki
                ),
                Align = Alignment.LEFT,
                Position = Position.Column(200.0f, 200.0f))
            |+ IconButton(L"wiki.openinbrowser", Icons.open_in_browser, 50.0f,
                fun () -> 
                    match current_page with
                    | WikiIndex -> openUrl("https://yavsrg.net/interlude/wiki/index.html")
                    | WikiPage p -> openUrl("https://yavsrg.net/interlude/wiki/" + p.Filename + ".html")
                    | Changelog -> openUrl("https://yavsrg.net/interlude/changelog.html")
                ,
                Position = Position.SliceRight(300.0f))

        do 
            Heading.scrollHandler <- fun w -> flow.Scroll(w.Bounds.Top - flow.Bounds.Top)
            if AutoUpdate.updateAvailable && not AutoUpdate.updateDownloaded then
                buttons |* IconButton(L"wiki.downloadupdate", Icons.download, 50.0f,
                fun () -> 
                    AutoUpdate.applyUpdate(fun () -> Notifications.system_feedback(Icons.system_notification, L"notification.update_installed.title", L"notification.update_installed.body"))
                    Notifications.system_feedback(Icons.system_notification, L"notification.update_installing.title", L"notification.update_installing.body")
                ,
                Position = Position.Column(500.0f, 270.0f))
        
        member private this.UpdateContent() =
            let con = StaticContainer(NodeType.None)
            let mutable y = 0.0f
            let max_width = 1400.0f
            let spacing = 30.0f
            match content with
            | Some paragraphs ->
                for paragraph in paragraphs do
                    let markdown = MarkdownUI.build max_width paragraph
                    markdown.Position <- Position.Box(0.0f, 0.0f, 0.0f, y, max_width, markdown.Height)
                    con.Add(markdown)
                    y <- y + markdown.Height + spacing
            | None -> ()
            flow <- ScrollContainer(con, y - spacing, Position = Position.Margin((Viewport.vwidth - 1400.0f) * 0.5f, 80.0f))
            flow.Init this
        
        override this.Init(parent: Widget) =
            base.Init parent
            buttons.Init this
            this.UpdateContent()
            page_changed <- this.UpdateContent
        
        override this.Update(elapsedTime, moved) =
            base.Update(elapsedTime, moved)
            buttons.Update(elapsedTime, moved)
            flow.Update(elapsedTime, moved)
            if Mouse.leftClick() || (!|"exit").Tapped() then
                this.Close()
        
        override this.Draw() = buttons.Draw(); flow.Draw()

    let show() = (Browser()).Show()

    let show_changelog() = 
        if current_page <> Changelog then
            load_resource Changelog
        (Browser()).Show()