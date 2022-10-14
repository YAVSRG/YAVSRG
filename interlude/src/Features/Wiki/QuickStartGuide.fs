namespace Interlude.Features.Wiki

open System.Net.Http
open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Interlude.Utils
open FSharp.Formatting.Markdown

module Wiki =

    let mutable private content = ""
    let mutable private page_changed : unit -> unit = ignore

    module PageLoader =

        type Request = string
        type Result = string

        let private httpClient =
            new HttpClient()
            |> fun w -> w.DefaultRequestHeaders.Add("User-Agent", "Interlude"); w

        let private worker = 
            { new Async.SingletonWorker<Request,Result>() with
                override this.Handle(req) = 
                    let url = "https://raw.githubusercontent.com/wiki/YAVSRG/Interlude/" + req + ".md"
                    httpClient.GetStringAsync url |> Async.AwaitTask |> Async.RunSynchronously
                override this.Callback(result) =
                    content <- result
                    sync(page_changed)
            }

        let handle page = worker.Request page

    do
        LinkHandler.add { 
            Prefix = "https://github.com/YAVSRG/Interlude/wiki/"
            Action = fun s -> s.Replace("https://github.com/YAVSRG/Interlude/wiki/", "") |> PageLoader.handle
        }
        PageLoader.handle "Home"

    type Browser() =
        inherit Dialog()

        let mutable flow = Unchecked.defaultof<Widget>
        
        member private this.UpdateContent() = 
            let content = Markdown.Parse content
            let markdown = MarkdownUI.build (this.Bounds.Width - 500.0f) content
            flow <- ScrollContainer(markdown, markdown.Height, Position = Position.Margin(100.0f, 100.0f).TrimRight(300.0f))
            flow.Init this
        
        override this.Init(parent: Widget) =
            base.Init parent
            this.UpdateContent()
            page_changed <- this.UpdateContent
        
        override this.Update(elapsedTime, moved) =
            base.Update(elapsedTime, moved)
            flow.Update(elapsedTime, moved)
            if Mouse.leftClick() || (!|"exit").Tapped() then
                this.Close()
        
        override this.Draw() = flow.Draw()

type QuickStartDialog(doc) =
    inherit Dialog()

    let mutable flow : Widget = Unchecked.defaultof<_>

    override this.Init(parent: Widget) =
        base.Init parent
        let markdown = MarkdownUI.build (this.Bounds.Width - 800.0f) doc
        flow <- ScrollContainer(markdown, markdown.Height, Position = Position.Margin(400.0f, 100.0f))
        flow.Init this

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        flow.Update(elapsedTime, moved)
        if Mouse.leftClick() || (!|"exit").Tapped() then
            this.Close()

    override this.Draw() = flow.Draw()

module Help =

    let quick_start_guide = getResourceText "QuickStart.md" |> Markdown.Parse
    let show_quick_guide() = (QuickStartDialog quick_start_guide).Show()

    let show_wiki() = (Wiki.Browser()).Show()