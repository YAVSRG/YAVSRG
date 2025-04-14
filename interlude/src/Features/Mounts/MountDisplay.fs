namespace Interlude.Features.Mounts

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.Library
open Prelude.Data.Library.Imports
open Interlude.UI
open Interlude.Options

type MountDisplay =

    static member HEIGHT = 150.0f
    static member ALL_HEIGHT = MountDisplay.HEIGHT * 4.0f + Style.PADDING * 12.0f + 70.0f
    static member WIDTH = 580.0f

    static member Create(game: MountedGameType, setting: Setting<Imports.MountedChartSource option>) : Container =

        let create_or_edit_button =
            Button(
                (fun () ->
                    if setting.Value.IsSome then
                        sprintf "%s %s" Icons.EDIT_2 (%"mount.edit")
                    else
                        sprintf "%s %s" Icons.LINK (%"mount.create")
                ),
                fun () ->
                    match setting.Value with
                    | Some mount -> EditMountPage(game, mount, setting).Show()
                    | None -> Mounts.detect_or_drop_folder game (fun path -> CreateMountPage(game, path, setting).Show())
            )

        FrameContainer
            .Create(create_or_edit_button.Position(Position.SliceB(50.0f).Shrink(5.0f)))
            .Fill(fun () -> if setting.Value.IsSome then game.Color.O2 else Colors.shadow_2)
            .Border(fun () -> if setting.Value.IsSome then game.Color else Colors.cyan_accent)
            .With(
                Text(game.ToString())
                    .Align(Alignment.CENTER)
                    .Position(Position.SliceT(0.0f, 60.0f)),
                Text(fun () ->
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
            )

    static member CreateAll() : NavigationContainer.Column =
        NavigationContainer.Column()
            .WrapNavigation(false)
            .With(
                MountDisplay.Create(MountedGameType.Osu, options.OsuMount)
                    .Position(Position.ShrinkT(70.0f).GridY(1, 4, Style.PADDING * 4.0f)),
                MountDisplay.Create(MountedGameType.Quaver, options.QuaverMount)
                    .Position(Position.ShrinkT(70.0f).GridY(2, 4, Style.PADDING * 4.0f)),
                MountDisplay.Create(MountedGameType.Etterna, options.EtternaMount)
                    .Position(Position.ShrinkT(70.0f).GridY(3, 4, Style.PADDING * 4.0f)),
                MountDisplay.Create(MountedGameType.Stepmania, options.StepmaniaMount)
                    .Position(Position.ShrinkT(70.0f).GridY(4, 4, Style.PADDING * 4.0f)),
                Text(%"imports.mount")
                    .Align(Alignment.CENTER)
                    .Position(Position.SliceT(0.0f, 60.0f))
            )