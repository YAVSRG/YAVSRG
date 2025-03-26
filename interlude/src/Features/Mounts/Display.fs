namespace Interlude.Features.Mounts

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.Library
open Interlude.UI

type MountControl(game: MountedGameType, setting: Setting<Imports.MountedChartSource option>) as this =
    inherit FrameContainer(NodeType.Container(fun _ -> Some this.VisibleButtons), Fill = K Colors.cyan.O2, Border = K Colors.cyan_accent)

    let create_button =
        Button(
            sprintf "%s %s" Icons.LINK (%"mount.create"),
            (fun () -> CreateMountPage(game, setting).Show())
        )
            .Position(Position.SliceB(50.0f).Shrink(5.0f))

    let edit_buttons =
        NavigationContainer.Row()
            .WrapNavigation(false)
            .Position(Position.SliceB(50.0f).Shrink(5.0f))
        |+ Button(
            sprintf "%s %s" Icons.EDIT_2 (%"mount.edit"),
            (fun () -> EditMountPage(game, setting).Show())
        )
            .Position(Position.SlicePercentL(0.5f))
        |+ Button(
            sprintf "%s %s" Icons.TRASH (%"mount.delete"),
            (fun () -> setting.Value <- None)
        )
            .Position(Position.SlicePercentR(0.5f))

    override this.Init(parent: Widget) =
        this
        |+ Text(
            game.ToString()
            )
            .Position(Position.SliceT(0.0f, 60.0f))
            .Align(Alignment.CENTER)
        |* Text(
            fun () ->
                match setting.Value with
                | Some s ->
                    [
                        match s.LastImported with
                        | Some date -> date.ToString()
                        | None -> "--"
                    ]
                    %> "mount.lastimported"
                | None -> %"mount.notlinked"
            )
            .Color(Colors.text_subheading)
            .Align(Alignment.CENTER)
            .Position(Position.SliceT(60.0f, 40.0f))

        base.Init parent
        create_button.Init this
        edit_buttons.Init this

    member private this.VisibleButtons: Widget =
        if setting.Value.IsSome then edit_buttons else create_button

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        this.VisibleButtons.Update(elapsed_ms, moved)

    override this.Draw() =
        base.Draw()
        this.VisibleButtons.Draw()