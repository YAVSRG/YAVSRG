namespace Interlude.UI.Menu

open System
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Prelude
open Interlude.UI

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

    let mutable is_current = false
    let mutable content: Widget = Unchecked.defaultof<_>

    member private this._content = content

    abstract member Title: string
    abstract member OnClose: unit -> unit
    abstract member OnDestroy: unit -> unit
    default this.OnDestroy() = ()

    abstract member OnReturnTo: unit -> unit
    default this.OnReturnTo() = ()

    abstract member Content : unit -> Widget
    abstract member Header : unit -> Widget
    default this.Header() = PageHeaderBase()

    override this.Update(elapsed_ms, moved) =
        if is_current && not content.Focused then
            Menu.Back()

        base.Update(elapsed_ms, moved)

    member this.Show(dir: PageEasing, returning: bool) =
        match dir with
        | PageEasing.None ->
            this.Position <- Position.Default
            this.SnapPosition()
        | _ ->
            this.Hide(dir.Reverse)
            this.SnapPosition()
            this.Position <- Position.Default

        Selection.clamp_to this

        if returning then
            this.OnReturnTo()

        content.Focus false
        is_current <- true

    member this.Hide(dir: PageEasing) =
        match dir with
        | PageEasing.Up ->
            this.Position <-
                { Position.Default with
                    Top = -1.0f %+ 0.0f
                    Bottom = 0.0f %+ 0.0f
                }
        | PageEasing.Down ->
            this.Position <-
                { Position.Default with
                    Top = 1.0f %+ 0.0f
                    Bottom = 2.0f %+ 0.0f
                }
        | PageEasing.Left ->
            this.Position <-
                { Position.Default with
                    Left = -1.0f %+ 0.0f
                    Right = 0.0f %+ 0.0f
                }
        | PageEasing.Right ->
            this.Position <-
                { Position.Default with
                    Left = 1.0f %+ 0.0f
                    Right = 2.0f %+ 0.0f
                }
        | PageEasing.None ->
            this.Position <-
                {
                    Left = -1.0f %+ 0.0f
                    Top = -1.0f %+ 0.0f
                    Right = 0.0f %+ 0.0f
                    Bottom = 0.0f %+ 0.0f
                }
            this.SnapPosition()

        is_current <- false

    override this.Init(parent: Widget) =
        content <- this.Content()
        this
        |+ this.Header()
        |* content

        this.Hide(this.Direction.Reverse)
        base.Init parent
        this.Show(this.Direction, false)

    abstract member Direction : PageEasing
    default this.Direction = PageEasing.Up

and PageHeaderBase() =
    inherit StaticWidget(NodeType.None)

    let titles = Menu.PageTitles() |> List.rev |> Array.ofList

    let GAP_BETWEEN_BOXES = 70.0f

    override this.Init parent =
        this.Position <- Position.Row(40.0f, 60.0f)
        base.Init parent

    override this.Draw() =
        let mutable x = this.Bounds.Left + PRETTY_MARGIN_X
        let mutable first = true
        for t in titles do
            let width = Text.measure (Style.font, t) * this.Bounds.Height * 0.6f
            let bounds = Rect.Box(x, this.Bounds.Top, width, this.Bounds.Height)
            Draw.rect (bounds.Expand(Style.PADDING * 2.0f, Style.PADDING)) Colors.shadow_2.O2
            Text.fill_b (Style.font, t, bounds, Colors.text, Alignment.CENTER)
            x <- x + width + GAP_BETWEEN_BOXES
            if not first then
                Text.fill_b (Style.font, Icons.ARROW_RIGHT, bounds.BorderLeft GAP_BETWEEN_BOXES, Colors.text_greyout, Alignment.CENTER)
            first <- false
        
and Menu(top_level: Page) as this =
    inherit Dialog()

    let MAX_PAGE_DEPTH = 12

    // Everything left of the current page is Some
    // Everything past the current page could be Some from an old backed out page
    let stack: Page option array = Array.create MAX_PAGE_DEPTH None
    let mutable namestack = []

    let back_button =
        IconButton(
            %"menu.back",
            Icons.ARROW_LEFT_CIRCLE,
            60.0f,
            Menu.Back,
            Position = Position.Box(0.0f, 1.0f, 5.0f, -70.0f, 160.0f, 60.0f)
        )

    let volume = Volume()
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
        let n = List.length namestack
        namestack <- page.Title :: namestack
        page.Init this

        match stack.[n] with
        | None -> ()
        | Some page -> page.OnDestroy()

        stack.[n] <- Some page

        if n > 0 then
            stack.[n - 1].Value.Hide(page.Direction)

    static member Back() =
        match _instance with
        | None -> ()
        | Some instance -> instance.Back()

    static member Exit() =
        match _instance with
        | None -> ()
        | Some instance -> instance.Exit()

    member private this.Back() =
        namestack <- List.tail namestack
        let n = List.length namestack
        let page = stack.[n].Value
        page.OnClose()
        page.Hide(page.Direction.Reverse)

        if n > 0 then
            stack.[n - 1].Value.Show(page.Direction.Reverse, true)

    member private this.Exit() =
        while namestack <> [] do
            this.Back()

    override this.Init(parent) =
        base.Init parent
        back_button.Init this
        volume.Init this
        exit_key.Init this
        this.ShowPage top_level

    override this.Draw() =
        back_button.Draw()
        volume.Draw()
        let mutable i = 0

        while i < MAX_PAGE_DEPTH && stack.[i].IsSome do
            stack.[i].Value.Draw()
            i <- i + 1

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        exit_key.Update(elapsed_ms, moved)

        let mutable i = 0

        while i < MAX_PAGE_DEPTH && stack.[i].IsSome do
            stack.[i].Value.Update(elapsed_ms, moved)
            i <- i + 1

        if List.isEmpty namestack then
            let mutable i = 0

            while i < MAX_PAGE_DEPTH && stack.[i].IsSome do
                stack.[i].Value.OnDestroy()
                i <- i + 1

            this.Close()

        back_button.Update(elapsed_ms, moved)
        volume.Update(elapsed_ms, moved)

        if (%%"screenshot").Tapped() then
            Toolbar.take_screenshot ()

    override this.Close() =
        base.Close()
        Selection.unclamp ()
        _instance <- None

    member private this.PageTitles() = namestack
    static member PageTitles() = 
        match _instance with
        | None -> []
        | Some menu -> menu.PageTitles()

type Page with
    member this.Show() = Menu.ShowPage this

//todo: change into multiple choice page, takes list of choices and actions
//then confirm is a static constructor
type ConfirmPage(prompt: string, yes: unit -> unit) =
    inherit Page()

    override this.Content() =
        page_container()
        |+ PageButton.Once(%"confirm.yes", fork Menu.Back yes).Pos(3)
        |+ PageButton.Once(%"confirm.no", Menu.Back).Pos(5)
        |+ Text(prompt, Align = Alignment.LEFT, Position = pretty_pos(0, 2, PageWidth.Full))
        :> Widget

    override this.Title = %"confirm"
    override this.OnClose() = ()
