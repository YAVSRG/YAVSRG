namespace Interlude.Features.Import.Mounts

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.Library
open Interlude.UI

type private MountControl(game: MountedGameType, setting: Setting<Imports.MountedChartSource option>) as this =
    inherit FrameContainer(NodeType.Container(fun _ -> Some this.VisibleButtons))

    let create_button =
        Button(
            sprintf "%s %s" Icons.LINK (%"mount.create"),
            (fun () -> CreateMountPage(game, setting).Show()),
            Position = Position.SliceBottom(50.0f).Margin 5.0f
        )

    let edit_buttons =
        NavigationContainer.Row(WrapNavigation = false, Position = Position.SliceBottom(50.0f).Margin 5.0f)
        |+ Button(
            sprintf "%s %s" Icons.EDIT_2 (%"mount.edit"),
            (fun () -> EditMountPage(game, setting).Show()),
            Position =
                { Position.Default with
                    Right = 0.5f %+ 0.0f
                }
        )
        |+ Button(
            sprintf "%s %s" Icons.TRASH (%"mount.delete"),
            (fun () -> setting.Value <- None),
            Position =
                { Position.Default with
                    Left = 0.5f %+ 0.0f
                }
        )

    override this.Init(parent: Widget) =
        this
        |+ Text(
            match game with
            | MountedGameType.Osu -> "osu!mania"
            | MountedGameType.Quaver -> "Quaver"
            | MountedGameType.Stepmania -> "Stepmania"
            | MountedGameType.Etterna -> "Etterna"
            , Position = Position.Row(0.0f, 60.0f)
            , Align = Alignment.CENTER
        )
        |* Text(
            fun () ->
                match setting.Value with
                | Some s ->
                    [
                        if s.LastImported = System.DateTime.UnixEpoch then
                            "--"
                        else
                            s.LastImported.ToString()
                    ]
                    %> "mount.lastimported"
                | None -> %"mount.notlinked"
            , Color = K Colors.text_subheading
            , Position = Position.Row(60.0f, 40.0f)
            , Align = Alignment.CENTER
        )

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
