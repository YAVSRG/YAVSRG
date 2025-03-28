namespace Interlude.Features.Skins

open Percyqaz.Common
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Prelude.Skins
open Prelude.Skins.Noteskins
open Prelude.Skins.HudLayouts
open Interlude.UI
open Interlude.Content

type ProblemCard =

    static member Create(msg: ValidationMessage, after_fix: unit -> unit) =
        match msg with
        | ValidationWarning w ->
            CalloutCard(
                Callout
                    .Normal
                    .Title(w.Element)
                    .Body(w.Message)
                |> fun c -> match w.SuggestedFix with Some fix -> c.Button(fix.Description, fork fix.Action after_fix) | None -> c
                , Colors.yellow_accent, Colors.yellow_accent.O2
            )
        | ValidationError e ->
            CalloutCard(
                Callout
                    .Normal
                    .Title(e.Element)
                    .Body(e.Message)
                |> fun c -> match e.SuggestedFix with Some fix -> c.Button(fix.Description, fork fix.Action after_fix) | None -> c
                , Colors.red_accent, Colors.red.O2
            )

module private ProblemList =

    let loader =
        { new Async.CancelQueueSeq<Storage * DynamicFlowContainer.Vertical<CalloutCard> * (unit -> unit), (unit -> unit)>() with
            override this.Process((storage, container, after_fix)) =
                match storage with
                | :? Noteskin as ns -> ns.Validate()
                | :? HudLayout as hud -> hud.Validate()
                | _ -> Seq.empty //todo: make Validate() an abstract member?
                |> Seq.map (fun msg -> fun () -> container.Add(ProblemCard.Create(msg, after_fix)))
            override this.Handle(action) = action()
        }

type ProblemList =

    static member Noteskin(noteskin: Noteskin) : {| Container: ScrollContainer<_>; Refresh: unit -> unit |} =

        let problems_list =
            DynamicFlowContainer.Vertical<CalloutCard>()
                .Spacing(Style.PADDING * 3.0f)

        let rec refresh () =
            problems_list.Clear()
            ProblemList.loader.Request(noteskin, problems_list, fun () -> Skins.reload_current_noteskin(); GameThread.defer refresh)

        {|
            Container =
                ScrollContainer(problems_list)
                    .Margin(Style.PADDING)
                    .Position(page_position(3, PAGE_BOTTOM - 3, PageWidth.Normal)
                )
            Refresh = refresh
        |}

    static member HUD(hud: HudLayout) : {| Container: ScrollContainer<_>; Refresh: unit -> unit |} =

        let problems_list =
            DynamicFlowContainer.Vertical<CalloutCard>()
                .Spacing(Style.PADDING * 3.0f)

        let rec refresh () =
            problems_list.Clear()
            ProblemList.loader.Request(hud, problems_list, fun () -> Skins.reload_current_hud(); GameThread.defer refresh)

        {|
            Container =
                ScrollContainer(problems_list)
                    .Margin(Style.PADDING)
                    .Position(page_position(0, PAGE_BOTTOM, PageWidth.Normal)
                )
            Refresh = refresh
        |}