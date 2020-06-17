namespace Interlude.UI

type NoteRenderer() =
    let a = 0

type ScreenPlay() =
    inherit Screen()

    override this.OnEnter(prev) =
        if (prev :? ScreenScore) then
            Screens.popScreen()
        else
            () //audio setup and stuff