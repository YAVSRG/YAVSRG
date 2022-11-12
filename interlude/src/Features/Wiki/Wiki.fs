namespace Interlude.Features.Wiki

open System.Diagnostics
open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude.Data
open Interlude.Utils
open Interlude.UI
open FSharp.Formatting.Markdown

module Wiki =

    type Page =
        | Wiki of string
        | Changelog

    let mutable private page_history = []
    let mutable private current_page = Wiki ""
    let mutable private content : MarkdownDocument = new MarkdownDocument([], Map.empty)
    let mutable private page_changed : unit -> unit = ignore

    let private page_loader =
        { new Async.Service<Page, MarkdownDocument>() with
            override this.Handle(req) =
                async {
                    let url = 
                        match req with
                        | Wiki p -> "https://raw.githubusercontent.com/wiki/YAVSRG/Interlude/" + p + ".md"
                        | Changelog -> "https://raw.githubusercontent.com/YAVSRG/Interlude/main/docs/changelog.md"
                    let! page = WebServices.download_string.RequestAsync(url)
                    return Markdown.Parse page
                }
        }

    let load_wiki_page (page: string) =
        page_history <- Wiki page :: page_history
        current_page <- Wiki page
        let page = 
            if page.Contains('#') then
                Heading.scrollTo <- page.Substring(page.LastIndexOf '#' + 1).Replace('-', ' ')
                page.Substring(0, page.LastIndexOf '#')
            else page
        page_loader.Request(Wiki page, fun md -> content <- md; sync page_changed)

    let load_changelog () =
        page_history <- Changelog :: page_history
        current_page <- Changelog
        page_loader.Request(Changelog, fun md -> content <- md; sync page_changed)

    do
        LinkHandler.add { 
            Prefix = "https://github.com/YAVSRG/Interlude/wiki/"
            Action = fun s -> s.Replace("https://github.com/YAVSRG/Interlude/wiki/", "") |> load_wiki_page
        }
        load_wiki_page "Home"

    type Browser() as this =
        inherit Dialog()

        let mutable flow = Unchecked.defaultof<ScrollContainer>

        let buttons = 
            SwitchContainer.Row(Position = Position.SliceTop(70.0f).Margin(250.0f, 10.0f))
            |+ IconButton(L"menu.back", Icons.back, 50.0f,
                fun () ->
                    match page_history with
                    | x :: y :: xs -> 
                        match y with
                        | Wiki p -> load_wiki_page p;
                        | Changelog -> load_changelog ()
                        page_history <- y :: xs
                    | _ -> this.Close()
                ,
                Position = Position.Column(0.0f, 200.0f))
            |+ IconButton(L"wiki.openinbrowser", Icons.open_in_browser, 50.0f,
                fun () -> 
                    match current_page with
                    | Wiki p -> openUrl("https://github.com/YAVSRG/Interlude/wiki/" + p)
                    | Changelog -> openUrl("https://github.com/YAVSRG/Interlude/releases")
                ,
                Position = Position.Column(200.0f, 300.0f))

        do 
            Heading.scrollHandler <- fun w -> flow.Scroll(w.Bounds.Top - flow.Bounds.Top)
            if AutoUpdate.updateAvailable && not AutoUpdate.updateDownloaded then
                buttons |* IconButton(L"wiki.downloadupdate", Icons.download, 50.0f,
                fun () -> 
                    AutoUpdate.applyUpdate(fun () -> Notifications.add (L"notification.update.installed", NotificationType.System))
                    Notifications.add (L"notification.update.installing", NotificationType.System)
                ,
                Position = Position.Column(500.0f, 270.0f))
        
        member private this.UpdateContent() =
            let markdown = MarkdownUI.build (this.Bounds.Width - 500.0f) content
            flow <- ScrollContainer(markdown, markdown.Height, Position = Position.Margin(250.0f, 100.0f))
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
            load_changelog()
        (Browser()).Show()