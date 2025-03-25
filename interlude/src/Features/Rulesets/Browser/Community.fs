namespace Interlude.Features.Rulesets.Browser

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay.Rulesets
open Prelude.Backbeat
open Prelude.Data
open Prelude.Data.Library
open Interlude.UI
open Interlude.Content

type private RulesetStatus =
    | NotInstalled
    | UpdateAvailable
    | UpToDate

type RulesetCard(id: string, ruleset: Ruleset) as this =
    inherit
        FrameContainer(
            NodeType.Button(fun () ->
                Style.click.Play()
                this.Install()
            ),
            Fill = (fun () -> if this.Focused then Colors.pink.O2 else Colors.shadow_2.O2),
            Border =
                (fun () ->
                    if this.Focused then
                        Colors.pink_accent
                    else
                        Colors.grey_2.O3
                )
        )

    let mutable status =
        if Rulesets.list () |> Seq.map fst |> Seq.contains id then
            if Ruleset.hash (Rulesets.by_id id) <> Ruleset.hash ruleset then
                UpdateAvailable
            else
                UpToDate
        else
            NotInstalled

    override this.Init(parent) =
        this
        |+ Text(ruleset.Name, Align = Alignment.LEFT).Position(Position.SliceT(50.0f).Shrink(10.0f, Style.PADDING))
        |+ Text(
            ruleset.Description,
            Align = Alignment.LEFT).Position(Position.ShrinkT(40.0f).Shrink(10.0f, Style.PADDING))
        |* MouseListener().Button(this)
        base.Init parent

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    member this.Install() =
        match status with
        | UpToDate -> ()
        | UpdateAvailable ->
            ConfirmPage(
                "Update this ruleset? (If you made changes yourself, they will be lost)",
                fun () ->
                    Rulesets.update id ruleset
                    status <- UpToDate
            )
                .Show()
        | NotInstalled ->
            Rulesets.install ruleset
            status <- UpToDate

    override this.Draw() =
        base.Draw()
        Render.rect (this.Bounds.SliceT(40.0f).SliceR(300.0f).Shrink(20.0f, 0.0f)) Colors.shadow_2.O2

        Text.fill_b (
            Style.font
            , match status with
              | NotInstalled -> Icons.DOWNLOAD + " Install"
              | UpdateAvailable -> Icons.DOWNLOAD + " Update available"
              | UpToDate -> Icons.CHECK + " Installed"
            , this.Bounds.SliceT(40.0f).SliceR(300.0f).Shrink(25.0f, Style.PADDING)
            , match status with
              | NotInstalled -> if this.Focused then Colors.text_yellow_2 else Colors.text
              | UpdateAvailable -> Colors.text_yellow_2
              | UpToDate -> Colors.text_green
            , Alignment.CENTER
        )

    member this.Name = ruleset.Name

    static member Filter(filter: FilterPart list) =
        fun (c: RulesetCard) ->
            List.forall
                (function
                | Impossible -> false
                | String str -> c.Name.ToLower().Contains(str)
                | _ -> true)
                filter

type RulesetSearch() as this =
    inherit Container(NodeType.Container(fun _ -> Some this.Items))

    let grid =
        GridFlowContainer<RulesetCard>(80.0f, 2, Spacing = (15.0f, 15.0f), WrapNavigation = false)

    let scroll =
        ScrollContainer(grid, Margin = Style.PADDING).Position(Position.ShrinkT(70.0f))

    let mutable loading = true
    let mutable failed = false

    override this.Init(parent) =
        WebServices.download_json (
            sprintf "https://raw.%s.com/YAVSRG/YAVSRG/main/backbeat/rulesets/rulesets.json" "githubusercontent",
            fun data ->
                match data with
                | WebResult.Ok (d: RulesetRepo) ->
                    GameThread.defer (fun () ->
                        for id in d.Rulesets.Keys do
                            grid.Add(RulesetCard(id, d.Rulesets.[id]))

                        loading <- false
                    )
                | _ ->
                    GameThread.defer (fun () ->
                        failed <- true
                        loading <- false
                    )
        )

        this
        |+ (SearchBox(
                Setting.simple "",
                (fun (f: FilterPart list) -> grid.Filter <- RulesetCard.Filter f),
                Position = Position.SliceT 60.0f
            )
            |+ LoadingIndicator.Border(fun () -> loading))
        |+ EmptyState(Icons.X, %"rulesets.browser.error").Conditional(fun () -> failed)
        |* scroll

        base.Init parent

    override this.Focusable = grid.Focusable

    member this.Items = grid