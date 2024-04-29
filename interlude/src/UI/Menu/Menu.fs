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
    inherit SlideContainer(NodeType.Container(fun _ -> this._content))

    let mutable is_current = false
    let mutable no_easing = false
    let mutable content: Widget option = None

    member private this._content = content |> Option.map (fun x -> x :> ISelection)

    abstract member Title: string
    abstract member OnClose: unit -> unit
    abstract member OnDestroy: unit -> unit
    default this.OnDestroy() = ()

    abstract member OnReturnTo: unit -> unit
    default this.OnReturnTo() = ()

    member this.Content(w: Widget) =
        this.Add(w)
        content <- Some w

    override this.Update(elapsed_ms, moved) =
        if is_current && not content.Value.Focused then
            Menu.Back()

        base.Update(elapsed_ms, moved)

    member this.Show(returning: bool) =
        this.Position <- Position.Default
        if no_easing then this.SnapPosition(); no_easing <- false

        Selection.clamp_to this

        if returning then
            this.OnReturnTo()

        content.Value.Focus false
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
            no_easing <- true

        is_current <- false

    override this.Init(parent: Widget) =
        if content.IsNone then
            failwithf "Call Content() to provide page content"

        this.Hide(this.Direction.Reverse)
        base.Init parent
        this.Show(false)

    abstract member Direction : PageEasing
    default this.Direction = PageEasing.Up
        

and Menu(top_level: Page) as this =
    inherit Dialog()

    let MAX_PAGE_DEPTH = 12

    // Everything left of the current page is Some
    // Everything past the current page could be Some from an old backed out page
    let stack: Page option array = Array.create MAX_PAGE_DEPTH None
    let mutable namestack = []
    let mutable name = ""

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

        page.Init this

        let n = List.length namestack
        namestack <- page.Title :: namestack
        name <- String.Join(" > ", List.rev namestack)

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
        name <- String.Join(" > ", List.rev namestack)
        let n = List.length namestack
        let page = stack.[n].Value
        page.OnClose()
        page.Hide(page.Direction.Reverse)

        if n > 0 then
            stack.[n - 1].Value.Show(true)

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

        Text.fill_b (Style.font, name, this.Bounds.SliceTop(100.0f).Shrink(20.0f), Colors.text, 0.0f)

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

type Page with
    member this.Show() = Menu.ShowPage this

//todo: change into multiple choice page, takes list of choices and actions
//then confirm is a static constructor
type ConfirmPage(prompt: string, yes: unit -> unit) =
    inherit Page()

    override this.Init(parent) =
        page_container()
        |+ PageButton.Once("confirm.yes", fork Menu.Back yes).Pos(3)
        |+ PageButton.Once("confirm.no", Menu.Back).Pos(5)
        |+ Text(prompt, Align = Alignment.LEFT, Position = pretty_pos(0, 2, PageWidth.Full))
        |> this.Content

        base.Init parent

    override this.Title = %"confirm"
    override this.OnClose() = ()
