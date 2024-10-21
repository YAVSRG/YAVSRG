namespace Interlude.Features.Rulesets.Edit

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay.Rulesets
open Interlude.UI

type EditWindowsPage(ruleset: Setting<Ruleset>) =
    inherit Page()

    let judgements = ruleset.Value.Judgements

    let early_window j : Setting<GameplayTime> =
        Setting.make 
            (fun v ->
                let mutable v = -(abs v) |> max -500.0f<ms / rate>
                for i = j - 1 downto 0 do
                    match judgements.[i].TimingWindows with
                    | Some (early, _) -> v <- min early v
                    | None -> ()
                for i = j + 1 to judgements.Length - 1 do
                    match judgements.[i].TimingWindows with
                    | Some (early, _) -> v <- max early v
                    | None -> ()
                let late = 
                    match judgements.[j].TimingWindows with
                    | Some (_, late) -> late
                    | None -> 0.0f<ms / rate>
                judgements.[j] <- { judgements.[j] with TimingWindows = Some(v, late) }
            )
            (fun () ->
                match judgements.[j].TimingWindows with 
                | Some (early, _) -> early
                | None -> 0.0f<ms / rate>
            )

    let late_window j : Setting<GameplayTime> =
        Setting.make 
            (fun v ->
                let mutable v = max 0.0f<ms / rate> v |> min 500.0f<ms / rate>
                for i = j - 1 downto 0 do
                    match judgements.[i].TimingWindows with
                    | Some (_, late) -> v <- max late v
                    | None -> ()
                for i = j + 1 to judgements.Length - 1 do
                    match judgements.[i].TimingWindows with
                    | Some (_, late) -> v <- min late v
                    | None -> ()
                let early = 
                    match judgements.[j].TimingWindows with
                    | Some (early, _) -> early
                    | None -> 0.0f<ms / rate>
                judgements.[j] <- { judgements.[j] with TimingWindows = Some(early, v) }
            )
            (fun () ->
                match judgements.[j].TimingWindows with 
                | Some (_, late) -> late
                | None -> 0.0f<ms / rate>
            )

    override this.Content() =
        let container = FlowContainer.Vertical<Widget>(PRETTYHEIGHT)

        for i, j in ruleset.Value.Judgements |> Array.indexed do

            let early_window = early_window i
            let late_window = late_window i

            let w = (PRETTYWIDTH - PRETTYTEXTWIDTH - 100.0f) * 0.5f

            let early = NumberEntry.create_uom "ms" early_window
            early.Position <- Position.SliceR(w).TranslateX(-w).ShrinkR(15.0f)
            let late = NumberEntry.create_uom "ms" late_window
            late.Position <- Position.SliceR(w).ShrinkL(15.0f)
            
            let c =
                NavigationContainer.Row()
                |+ Button(
                    (fun () -> if judgements.[i].TimingWindows.IsSome then Icons.X_CIRCLE else Icons.PLUS_CIRCLE),
                    (fun () -> 
                        if judgements.[i].TimingWindows.IsSome then 
                            judgements.[i] <- { judgements.[i] with TimingWindows = None } 
                        else 
                            early_window.Set (-infinityf * 1.0f<_>)
                            late_window.Set (infinityf * 1.0f<_>)
                    ),
                    Position = Position.SliceL 100.0f)
                |+ early.Conditional(fun () -> judgements.[i].TimingWindows.IsSome)
                |+ late.Conditional(fun () -> judgements.[i].TimingWindows.IsSome)
            
            container.Add (PageSetting(j.Name, c))

        page_container()
        |+ ScrollContainer(container)
            .Pos(0, PAGE_BOTTOM)
        :> Widget
        
    override this.Title = %"rulesets.edit.windows"
    override this.OnClose() =
        ruleset.Set { ruleset.Value with Judgements = judgements }
