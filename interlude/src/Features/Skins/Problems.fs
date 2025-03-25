namespace Interlude.Features.Skins

open Percyqaz.Common
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Prelude.Skins
open Prelude.Skins.Noteskins
open Prelude.Skins.HudLayouts
open Interlude.UI
open Interlude.Content

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
        { new Async.CancelQueueSeq<Storage * DynamicFlowContainer.Vertical<CalloutCard> * (unit -> unit), (unit -> unit)>() with
            override this.Process((storage, container, after_fix)) =
                match storage with
                | :? Noteskin as ns -> ns.Validate()
                | :? HudLayout as hud -> hud.Validate()
                | _ -> Seq.empty //todo: make Validate() an abstract member
                |> Seq.map (fun msg -> fun () -> container |* problem_card msg after_fix)
            override this.Handle(action) = action()
        }

    let create_noteskin (noteskin: Noteskin) : Widget * (unit -> unit) =
        let problems_list =
            DynamicFlowContainer.Vertical<CalloutCard>(Spacing = 15.0f)

        let rec refresh () =
            problems_list.Clear()
            problems_loader.Request(noteskin, problems_list, fun () -> Skins.reload_current_noteskin(); GameThread.defer refresh)

        ScrollContainer(
            problems_list,
            Margin = Style.PADDING).Position(page_position(3, PAGE_BOTTOM - 3, PageWidth.Normal)), refresh

    let create_hud (hud: HudLayout) : Widget * (unit -> unit) =
        let problems_list =
            DynamicFlowContainer.Vertical<CalloutCard>(Spacing = 15.0f)

        let rec refresh () =
            problems_list.Clear()
            problems_loader.Request(hud, problems_list, fun () -> Skins.reload_current_hud(); GameThread.defer refresh)

        ScrollContainer(
            problems_list,
            Margin = Style.PADDING).Position(page_position(0, PAGE_BOTTOM, PageWidth.Normal)), refresh