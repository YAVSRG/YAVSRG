namespace Interlude.Features.Skins.EditNoteskin

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Skins.Noteskins
open Interlude.Content
open Interlude.Options
open Interlude.UI
open Interlude.Features.Gameplay
open Interlude.Features.Skins

type NoteSettingsPage() =
    inherit Page()

    let data = Content.NoteskinConfig

    let use_rotation = Setting.simple data.UseRotation

    let note_animation_time =
        data.AnimationFrameTime |> Setting.bounded(10.0f<ms / rate>, 1000.0f<ms / rate>) |> Setting.roundf_uom 2

    let keymode: Setting<Keymode> = Setting.simple(SelectedChart.keymode())

    let note_rotations = data.Rotations

    let rotate_picker keycount i =
        let k = int keycount - 3

        Setting.make (fun v -> note_rotations.[k].[i] <- v) (fun () -> note_rotations.[k].[i]) |> Setting.round 1

    let mutable note_colors = data.NoteColors

    let color_picker keycount i =
        let k = if note_colors.UseGlobalColors then 0 else int keycount - 2
        Setting.make (fun v -> note_colors.Colors.[k].[i] <- v) (fun () -> note_colors.Colors.[k].[i])

    let NOTE_SCALE = PAGE_ITEM_HEIGHT * 1.5f - Style.PADDING * 2.0f

    let rotations, refresh_rotations =
        refreshable_row
            (fun () -> int keymode.Value)
            (fun i k ->
                let x = NOTE_SCALE * -0.5f * float32 k
                let n = float32 i

                NoteRotationPicker(rotate_picker keymode.Value i)
                    .Position(
                        { Position.DEFAULT with
                            Left = 0.5f %+ (x + NOTE_SCALE * n)
                            Right = 0.5f %+ (x + NOTE_SCALE * n + NOTE_SCALE)
                        }
                    )
            )

    let colors, refresh_colors =
        refreshable_row
            (fun () -> ColorScheme.count (int keymode.Value) note_colors.Style)
            (fun i k ->
                let x = NOTE_SCALE * -0.5f * float32 k
                let n = float32 i

                NoteColorPicker(color_picker keymode.Value i, note_colors.Style, i)
                    .Position(
                        { Position.DEFAULT with
                            Left = 0.5f %+ (x + NOTE_SCALE * n)
                            Right = 0.5f %+ (x + NOTE_SCALE * n + NOTE_SCALE)
                        }
                    )
            )

    member this.SaveChanges() : unit =
        Skins.save_noteskin_config
            { Content.NoteskinConfig with
                AnimationFrameTime = note_animation_time.Value
                NoteColors = note_colors
                Rotations = note_rotations
                UseRotation = use_rotation.Value
            }

    override this.Content() : Widget =
        this.OnClose(this.SaveChanges)

        page_container()
            .With(
                PageSetting(
                    %"generic.keymode",
                    SelectDropdown.FromEnum(
                        keymode
                        |> Setting.trigger(fun _ ->
                            refresh_colors()
                            refresh_rotations()
                        )
                    )
                )
                    .Pos(0),
                PageSetting(
                    %"noteskin.globalcolors",
                    Checkbox(
                        Setting.make
                            (fun v -> note_colors <- { note_colors with UseGlobalColors = v })
                            (fun () -> note_colors.UseGlobalColors)
                        |> Setting.trigger(ignore >> refresh_colors)
                    )
                )
                    .Help(Help.Info("noteskin.globalcolors"))
                    .Pos(3),
                PageSetting(
                    %"noteskin.colorstyle",
                    SelectDropdown(
                        [|
                            ColorScheme.Column, %"noteskin.colorstyle.column"
                            ColorScheme.Chord, %"noteskin.colorstyle.chord"
                            ColorScheme.DDR, %"noteskin.colorstyle.ddr"
                        |],
                        Setting.make
                            (fun v -> note_colors <- { note_colors with Style = v })
                            (fun () -> note_colors.Style)
                        |> Setting.trigger(ignore >> refresh_colors)
                    )
                )
                    .Help(Help.Info("noteskin.colorstyle"))
                    .Pos(5),
                PageSetting(%"noteskin.notecolors", colors).Pos(7, 3, PageWidth.Full),
                PageSetting(%"noteskin.userotation", Checkbox use_rotation)
                    .Help(Help.Info("noteskin.userotation"))
                    .Pos(10),
                PageSetting(%"noteskin.rotations", rotations).Pos(12, 3, PageWidth.Full),
                PageSetting(%"noteskin.animationtime", Slider(Setting.uom note_animation_time))
                    .Help(Help.Info("noteskin.animationtime"))
                    .Pos(16),
                HotkeyListener("reload_content", fun () -> SkinHotReload.hot_reload_noteskin(Some NoteSettingsPage))
            )
        :> Widget

    override this.Title = %"noteskin.notes"
