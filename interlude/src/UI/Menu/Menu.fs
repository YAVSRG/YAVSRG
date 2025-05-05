namespace Interlude.UI

open System
open Percyqaz.Common
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Prelude
open Interlude.Options

[<RequireQualifiedAccess>]
type PageEasing =
    | Up
    | Down
    | Left
    | Right
    | None
    member this.Reverse =
        match this with
        | Up -> Down
        | Down -> Up
        | Left -> Right
        | Right -> Left
        | None -> None

[<AbstractClass>]
type Page() as this =
    inherit SlideContainer(NodeType.Container(fun _ -> Some this._content))

    let EASING_SCALE = 0.1f

    let mutable is_current = false
    let mutable content: Widget = Unchecked.defaultof<_>

    let on_close_ev = Event<unit>()
    let on_close = on_close_ev.Publish

    let on_destroy_ev = Event<unit>()
    let on_destroy = on_destroy_ev.Publish

    member val Opacity : Animation.Fade = Animation.Fade(0.0f, Target = 1.0f) with get

    member internal this.OnClose() : unit = on_close_ev.Trigger()
    member internal this.OnDestroy() : unit = on_destroy_ev.Trigger()

    member this.OnClose(action: unit -> unit) = on_close.Add action
    member this.DisposeOnClose([<ParamArray>] resources: IDisposable array) = on_close.Add (fun () -> resources |> Array.iter _.Dispose())
    member this.WithOnClose(action: unit -> unit) = this.OnClose(action); this

    member this.DisposeOnDestroy([<ParamArray>] resources: IDisposable array) = on_destroy.Add (fun () -> resources |> Array.iter _.Dispose())

    member private this._content = content

    abstract member Title: string

    abstract member OnReturnFromNestedPage: unit -> unit
    default this.OnReturnFromNestedPage() = ()

    abstract member OnEnterNestedPage: unit -> unit
    default this.OnEnterNestedPage() = ()

    abstract member Content : unit -> Widget

    abstract member Header : unit -> Widget
    default this.Header() = PageHeaderBase()

    abstract member Footer : unit -> Widget
    default this.Footer() =
        Button(Icons.ARROW_LEFT_CIRCLE + " " + %"menu.back", Menu.Back)
            .Position(Position.SliceB(80.0f).SliceY(55.0f).ShrinkL(10.0f).SliceL(180.0f))
        |> OverlayContainer
        :> Widget

    override this.Update(elapsed_ms, moved) =
        if is_current && not content.Focused then
            Menu.Back()
        this.Opacity.Update elapsed_ms
        if this.Opacity.Value > 0.01f then
            base.Update(elapsed_ms, moved)

    member this.Show(dir: PageEasing) =
        match dir with
        | PageEasing.None ->
            this.Position <- Position.DEFAULT
            this.SnapPosition()
        | _ ->
            this.Hide(dir.Reverse)
            this.SnapPosition()
            this.Position <- Position.DEFAULT
        this.Opacity.Target <- 1.0f

        Selection.clamp_to this

        content.Focus false
        is_current <- true

    member this.Hide(dir: PageEasing) =
        match dir with
        | PageEasing.Up ->
            this.Position <-
                { Position.DEFAULT with
                    Top = (0.0f - EASING_SCALE) %+ 0.0f
                    Bottom = (1.0f - EASING_SCALE) %+ 0.0f
                }
        | PageEasing.Down ->
            this.Position <-
                { Position.DEFAULT with
                    Top = (0.0f + EASING_SCALE) %+ 0.0f
                    Bottom = (1.0f + EASING_SCALE) %+ 0.0f
                }
        | PageEasing.Left ->
            this.Position <-
                { Position.DEFAULT with
                    Left = (0.0f - EASING_SCALE) %+ 0.0f
                    Right = (1.0f - EASING_SCALE) %+ 0.0f
                }
        | PageEasing.Right ->
            this.Position <-
                { Position.DEFAULT with
                    Left = (0.0f + EASING_SCALE) %+ 0.0f
                    Right = (1.0f + EASING_SCALE) %+ 0.0f
                }
        | PageEasing.None -> ()
        this.Opacity.Target <- 0.0f

        is_current <- false

    override this.Init(parent: Widget) =
        content <- NavigationContainer.Column().With(this.Content(), this.Footer(), this.Header())
        this.Add(content)

        this.Hide(this.Direction.Reverse)
        base.Init parent
        this.Show(this.Direction)

    abstract member Direction : PageEasing
    default this.Direction = PageEasing.Up

and PageHeaderBase() =
    inherit StaticWidget(NodeType.None)

    let titles = Menu.PageTitles() |> List.rev |> Array.ofList

    let GAP_BETWEEN_BOXES = 70.0f

    override this.Init parent =
        this.Position <- Position.SliceT(40.0f, 60.0f)
        base.Init parent

    override this.Draw() =
        let mutable x = this.Bounds.Left + PAGE_MARGIN_X
        let mutable first = true
        for t in titles do
            let width = Text.measure (Style.font, t) * this.Bounds.Height * 0.6f
            let bounds = Rect.FromSize(x, this.Bounds.Top, width, this.Bounds.Height)
            Render.rect (bounds.Expand(Style.PADDING * 2.0f, Style.PADDING)) Colors.shadow_2.O2
            Text.fill_b (Style.font, t, bounds, Colors.text, Alignment.CENTER)
            x <- x + width + GAP_BETWEEN_BOXES
            if not first then
                Text.fill_b (Style.font, Icons.ARROW_RIGHT, bounds.BorderL GAP_BETWEEN_BOXES, Colors.text_greyout, Alignment.CENTER)
            first <- false

and Menu(top_level: Page) as this =
    inherit Dialog()

    let MAX_PAGE_DEPTH = 12

    // Everything left of the current page is Some
    // Everything past the current page could be Some from an old backed out page
    let stack: Page option array = Array.create MAX_PAGE_DEPTH None
    let mutable namestack = []
    let mutable nest_level = 0

    let volume = VolumeSlider()
    let exit_key = HotkeyHoldAction("exit", (fun () -> Selection.up false), Menu.Exit)

    static let mutable _instance = None
    do _instance <- Some this

    static member ShowPage(page: Page) =
        match _instance with
        | None -> Menu(page).Show()
        | Some instance -> instance.ShowPage page

    static member ShowPage(page: unit -> #Page) =
        match _instance with
        | None -> Menu(page ()).Show()
        | Some instance -> instance.ShowPage(page ())

    member private this.ShowPage(page: Page) =
        namestack <- page.Title :: namestack
        page.Init this

        match stack.[nest_level] with
        | None -> ()
        | Some page -> page.OnDestroy()

        stack.[nest_level] <- Some page

        if nest_level > 0 then
            stack.[nest_level - 1].Value.Hide(page.Direction)
            stack.[nest_level - 1].Value.OnEnterNestedPage()

        nest_level <- nest_level + 1

    static member Back() =
        match _instance with
        | None -> ()
        | Some instance -> instance.Back()

    static member Exit() =
        match _instance with
        | None -> ()
        | Some instance -> instance.Exit()

    member private this.Back() =
        if nest_level > 0 then
            namestack <- List.tail namestack
            nest_level <- nest_level - 1
            let page = stack.[nest_level].Value
            page.OnClose()
            page.Hide(page.Direction.Reverse)

            if nest_level > 0 then
                stack.[nest_level - 1].Value.Show(page.Direction.Reverse)
                stack.[nest_level - 1].Value.OnReturnFromNestedPage()

    member private this.Exit() =
        while namestack <> [] do
            this.Back()

    override this.Init(parent) =
        base.Init parent
        volume.Init this
        exit_key.Init this
        this.ShowPage top_level
        if options.MenusMuffleSong.Value then Song.set_low_pass 1.0f

    override this.Draw() =
        let mutable i = 0

        while i < MAX_PAGE_DEPTH && stack.[i].IsSome do
            let a = stack.[i].Value.Opacity.Alpha
            if a < 1 then ()
            elif a < 255 then
                let before_alpha = Render.alpha_multiplier_begin stack.[i].Value.Opacity.Value
                stack.[i].Value.Draw()
                Render.alpha_multiplier_restore before_alpha
            else
                stack.[i].Value.Draw()
            i <- i + 1
        volume.Draw()

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if (%%"screenshot").Pressed() then
            Toolbar.take_screenshot ()
        elif (%%"reload_content").Pressed() then
            Notifications.action_feedback(Icons.IMAGE, %"notification.reload_content_hint", "")

        if nest_level > 0 then
            stack.[nest_level - 1].Value.Update(elapsed_ms, moved)

        volume.Update(elapsed_ms, moved)

        exit_key.Update(elapsed_ms, moved)

        Input.finish_frame_events()

        do
            let mutable i = 0

            while i < MAX_PAGE_DEPTH && stack.[i].IsSome do
                if i <> nest_level - 1 then stack.[i].Value.Update(elapsed_ms, moved)
                i <- i + 1

        if List.isEmpty namestack then
            let mutable i = 0

            while i < MAX_PAGE_DEPTH && stack.[i].IsSome do
                stack.[i].Value.OnDestroy()
                i <- i + 1

            this.Close()

    override this.Close() =
        base.Close()
        Selection.unclamp ()
        _instance <- None
        Song.set_low_pass 0.0f

    member private this.PageTitles() = namestack
    static member PageTitles() =
        match _instance with
        | None -> []
        | Some menu -> menu.PageTitles()

type Page with
    member this.Show() = Menu.ShowPage this

type ConfirmPage(prompt: string, options: (string * (unit -> unit)) array) =
    inherit Page()

    new(prompt, if_yes) = ConfirmPage(prompt, [|%"confirm.yes", if_yes; %"confirm.no", ignore|])

    override this.Content() =
        let lines = prompt.Split("\n", System.StringSplitOptions.TrimEntries)

        page_container()
            .With(seq {
                for i, (label, action) in Seq.indexed options do
                    yield PageButton.Once(label, fork action Menu.Back).Pos(lines.Length * 2 + 1 + i * 2)
            } : Widget seq)
            .With(seq {
                for i, line in Seq.indexed lines do
                    yield Text(line)
                        .Align(Alignment.LEFT)
                        .Position(page_position(i * 2, 2, PageWidth.Full).Shrink(if i > 0 then Style.PADDING else 0.0f))
            } : Widget seq)

    override this.Title = %"confirm"