namespace Interlude.UI

type Screen() =
    inherit Widget()
    member this.OnEnter(prev: Screen) = ()
    member this.OnExit(next: Screen) = ()

type ScreenMenu() =
    inherit Screen()

type ScreenContainer() =
    inherit Widget()

    let mutable dialogs = []
    let mutable current = new ScreenMenu() :> Screen
    let mutable screens = [current]

    let AddScreen(s: Screen) =
        screens <- s :: screens
        current.OnExit(s)
        s.OnEnter(current)
        current <- s

    let RemoveScreen() =
        screens <- List.tail screens
        match List.tryHead screens with
        | None -> ()
        | Some s ->
            current.OnExit(s)
            s.OnEnter(current)