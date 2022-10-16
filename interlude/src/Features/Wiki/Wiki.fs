namespace Interlude.Features.Wiki

open System.Diagnostics
open System.Net.Http
open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Interlude.Utils
open FSharp.Formatting.Markdown

module Wiki =

    let mutable private page_history = []
    let mutable private page_url = ""
    let mutable private content = ""
    let mutable private page_changed : unit -> unit = ignore

    module PageLoader =

        type Request = string
        type Result = string

        let private httpClient =
            new HttpClient()
            |> fun w -> w.DefaultRequestHeaders.Add("User-Agent", "Interlude"); w

        let private worker = 
            { new Async.SwitchService<Request,Result>() with
                override this.Handle(req) = 
                    let url = "https://raw.githubusercontent.com/wiki/YAVSRG/Interlude/" + req + ".md"
                    httpClient.GetStringAsync url |> Async.AwaitTask |> Async.RunSynchronously
                override this.Callback(result) =
                    content <- result
                    sync(page_changed)
            }

        let handle (page: string) =
            page_history <- page :: page_history
            page_url <- page
            let page = 
                if page.Contains('#') then
                    Heading.scrollTo <- page.Substring(page.LastIndexOf '#' + 1).Replace('-', ' ')
                    page.Substring(0, page.LastIndexOf '#')
                else page
            worker.Request page

    do
        LinkHandler.add { 
            Prefix = "https://github.com/YAVSRG/Interlude/wiki/"
            Action = fun s -> s.Replace("https://github.com/YAVSRG/Interlude/wiki/", "") |> PageLoader.handle
        }
        PageLoader.handle "Home"

    type Browser() as this =
        inherit Dialog()

        let mutable flow = Unchecked.defaultof<ScrollContainer>

        let buttons = 
            SwitchContainer.Row(Position = Position.SliceTop(70.0f).Margin(250.0f, 10.0f))
            |+ IconButton(L"menu.back", Interlude.UI.Icons.back, 50.0f,
                fun () ->
                    match page_history with
                    | x :: y :: xs -> PageLoader.handle y; page_history <- y :: xs
                    | _ -> this.Close()
                ,
                Position = Position.Column(0.0f, 200.0f))
            |+ IconButton(L"wiki.openinbrowser", Interlude.UI.Icons.wiki2, 50.0f,
                fun () -> 
                    try Process.Start (ProcessStartInfo ("https://github.com/YAVSRG/Interlude/wiki/" + page_url, UseShellExecute=true)) |> ignore
                    with err -> Logging.Debug ("Failed to open wiki page in browser: " + page_url, err)
                ,
                Position = Position.Column(200.0f, 300.0f))

        do Heading.scrollHandler <- fun w -> flow.Scroll(w.Bounds.Top - flow.Bounds.Top)
        
        member private this.UpdateContent() = 
            let content = Markdown.Parse content
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