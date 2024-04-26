namespace Interlude.Features.EditNoteskin

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude.Content
open Prelude.Content.Noteskins
open Interlude.UI
open Interlude.UI.Menu

module Problems =

    let private problem_card (msg: ValidationMessage) (after_fix: unit -> unit) =
        match msg with
        | ValidationWarning w ->
            CalloutCard(
                Callout
                    .Normal
                    .Title(w.Element)
                    .Body(w.Message)
                |> fun c -> match w.SuggestedFix with Some fix -> c.Button(fix.Description, fork fix.Action after_fix) | None -> c
                , Colors.yellow_accent, Colors.yellow_accent.O2)
        | ValidationError e ->
            CalloutCard(
                Callout
                    .Normal
                    .Title(e.Element)
                    .Body(e.Message)
                |> fun c -> match e.SuggestedFix with Some fix -> c.Button(fix.Description, fork fix.Action after_fix) | None -> c
                , Colors.red_accent, Colors.red.O2)

    let problems_loader = 
        { new Async.SwitchServiceSeq<Noteskin * DynamicFlowContainer.Vertical<CalloutCard> * (unit -> unit), (unit -> unit)>() with 
            override this.Process((noteskin, container, after_fix)) =
                noteskin.Validate() |> Seq.map (fun msg -> fun () -> container |* problem_card msg after_fix)
            override this.Handle(action) = action()
        }

    let create_list (noteskin: Noteskin) : Widget * (unit -> unit) =
        let problems_list =
            DynamicFlowContainer.Vertical<CalloutCard>(Spacing = 15.0f)

        let rec refresh () =
            problems_list.Clear()
            problems_loader.Request(noteskin, problems_list, fun () -> sync refresh)

        ScrollContainer(
            problems_list,
            Margin = Style.PADDING,
            Position = pretty_pos(3, PAGE_BOTTOM - 3, PageWidth.Normal)
        ), refresh