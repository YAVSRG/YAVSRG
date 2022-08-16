namespace Interlude.Features.Wiki

open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Interlude.Utils
open FSharp.Formatting.Markdown

// todo: make this read from/integrate with the online wiki

type QuickStartDialog(doc) =
    inherit Dialog()

    let markdown = MarkdownUI.build doc
    let flow = ScrollContainer(markdown.Body, markdown.Height + markdown.LHeight, Position = Position.Margin(400.0f, 100.0f))

    override this.Init(parent: Widget) =
        base.Init parent
        flow.Init this

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        flow.Update(elapsedTime, moved)
        if Mouse.leftClick() || (!|"exit").Tapped() then
            this.Close()

    override this.Draw() = flow.Draw()

module QuickStartGuide =

    let doc = 
        getResourceText "QuickStart.md"
        |> Markdown.Parse

    let show_help() = (QuickStartDialog doc).Show()