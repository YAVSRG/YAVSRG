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

    let mutable private page_history = []
    let mutable private page_url = ""
    let mutable private content : MarkdownDocument = new MarkdownDocument([], Map.empty)
    let mutable private page_changed : unit -> unit = ignore

    let private page_loader =
        { new Async.Service<string, MarkdownDocument>() with
            override this.Handle(req) =
                async {
                    let url = "https://raw.githubusercontent.com/wiki/YAVSRG/Interlude/" + req + ".md"
                    let! page = WebServices.download_string.RequestAsync(url)
                    return Markdown.Parse page
                }
        }

    let load_page (page: string) =
        page_history <- page :: page_history
        page_url <- page
        let page = 
            if page.Contains('#') then
                Heading.scrollTo <- page.Substring(page.LastIndexOf '#' + 1).Replace('-', ' ')
                page.Substring(0, page.LastIndexOf '#')
            else page
        page_loader.Request(page, fun md -> content <- md; sync(page_changed))

    do
        LinkHandler.add { 
            Prefix = "https://github.com/YAVSRG/Interlude/wiki/"
            Action = fun s -> s.Replace("https://github.com/YAVSRG/Interlude/wiki/", "") |> load_page
        }
        load_page "Home"

    type Browser() as this =
        inherit Dialog()

        let mutable flow = Unchecked.defaultof<ScrollContainer>

        let buttons = 
            SwitchContainer.Row(Position = Position.SliceTop(70.0f).Margin(250.0f, 10.0f))
            |+ IconButton(L"menu.back", Icons.back, 50.0f,
                fun () ->
                    match page_history with
                    | x :: y :: xs -> load_page y; page_history <- y :: xs
                    | _ -> this.Close()
                ,
                Position = Position.Column(0.0f, 200.0f))
            |+ IconButton(L"wiki.openinbrowser", Icons.open_in_browser, 50.0f,
                fun () -> 
                    try Process.Start (ProcessStartInfo ("https://github.com/YAVSRG/Interlude/wiki/" + page_url, UseShellExecute=true)) |> ignore
                    with err -> Logging.Debug ("Failed to open wiki page in browser: " + page_url, err)
                ,
                Position = Position.Column(200.0f, 300.0f))

        do Heading.scrollHandler <- fun w -> flow.Scroll(w.Bounds.Top - flow.Bounds.Top)
        
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