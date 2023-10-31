namespace Interlude.Features.OptionsMenu.Noteskins

open Prelude.Charts.Tools.NoteColors
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.Data.Content
open Interlude.Content
open Interlude.Options
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Menu

type NoteColorPicker(color: Setting<byte>, style: ColorScheme, index: int) as this =
    inherit StaticContainer(NodeType.Leaf)

    let sprite = getTexture "note"
    let n = byte sprite.Rows

    let fd () =
        Setting.app (fun x -> (x + n - 1uy) % n) color
        Style.click.Play()

    let bk () =
        Setting.app (fun x -> (x + 1uy) % n) color
        Style.click.Play()

    do
        this
        |+ Tooltip(
            Callout.Normal
                .Title(sprintf "%s: %O" (L "noteskins.edit.notecolors.name") style)
                .Body(L(sprintf "noteskins.edit.notecolors.%s.%i" (style.ToString().ToLower()) index))
        )
        |* Clickable(
            (fun () ->
                (if not this.Selected then
                     this.Select())

                fd ()),
            OnHover =
                fun b ->
                    if b && not this.Focused then
                        this.Focus()
        )

    override this.OnFocus() =
        Style.hover.Play()
        base.OnFocus()

    override this.Draw() =
        base.Draw()

        if this.Selected then
            Draw.rect this.Bounds Colors.pink_accent.O2
        elif this.Focused then
            Draw.rect this.Bounds Colors.yellow_accent.O2

        Draw.quad (Quad.ofRect this.Bounds) (Quad.color Color.White) (Sprite.with_uv (3, int color.Value) sprite)

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)

        if this.Selected then
            if (!| "up").Tapped() then
                fd ()
            elif (!| "down").Tapped() then
                bk ()
            elif (!| "left").Tapped() then
                bk ()
            elif (!| "right").Tapped() then
                fd ()

type ColorSettingsPage() as this =
    inherit Page()

    let data = Noteskins.Current.config
    let keycount = Setting.simple options.KeymodePreference.Value
    let mutable noteColors = data.NoteColors

    let g keycount i =
        let k = if noteColors.UseGlobalColors then 0 else int keycount - 2
        Setting.make (fun v -> noteColors.Colors.[k].[i] <- v) (fun () -> noteColors.Colors.[k].[i])

    let NOTE_WIDTH = 120.0f

    let colors, refreshColors =
        refreshRow (fun () -> ColorScheme.count (int keycount.Value) noteColors.Style) (fun i k ->
            let x = -60.0f * float32 k
            let n = float32 i

            NoteColorPicker(
                g keycount.Value i,
                noteColors.Style,
                i,
                Position =
                    { Position.Default with
                        Left = 0.5f %+ (x + NOTE_WIDTH * n)
                        Right = 0.5f %+ (x + NOTE_WIDTH * n + NOTE_WIDTH) }
            ))

    do
        this.Content(
            column ()
            |+ PageSetting(
                "noteskins.edit.globalcolors",
                Selector<_>
                    .FromBool(
                        Setting.make (fun v -> noteColors <- { noteColors with UseGlobalColors = v }) (fun () ->
                            noteColors.UseGlobalColors)
                        |> Setting.trigger (ignore >> refreshColors)
                    )
            )
                .Pos(200.0f)
                .Tooltip(Tooltip.Info("noteskins.edit.globalcolors"))
            |+ PageSetting(
                "generic.keymode",
                Selector<Keymode>
                    .FromEnum(keycount |> Setting.trigger (ignore >> refreshColors))
            )
                .Pos(270.0f)
            |+ PageSetting(
                "noteskins.edit.colorstyle",
                Selector.FromEnum(
                    Setting.make (fun v -> noteColors <- { noteColors with Style = v }) (fun () -> noteColors.Style)
                    |> Setting.trigger (ignore >> refreshColors)
                )
            )
                .Pos(370.0f)
                .Tooltip(Tooltip.Info("noteskins.edit.colorstyle"))
            |+ PageSetting("noteskins.edit.notecolors", colors)
                .Pos(470.0f, Viewport.vwidth - 200.0f, NOTE_WIDTH)
        )

    override this.Title = L "noteskins.edit.colors.name"

    override this.OnClose() =
        Noteskins.Current.changeConfig
            { Noteskins.Current.config with
                NoteColors = noteColors }
