namespace Interlude.Features.OptionsMenu.Themes

open Prelude.Gameplay.NoteColors
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.Data.Themes
open Interlude.Content
open Interlude.Options
open Interlude.UI.Menu

type NoteColorPicker(color: Setting<byte>) as this =
    inherit StaticContainer(NodeType.Leaf)
    
    let sprite = getTexture "note"
    let n = byte sprite.Rows
    
    let fd() = Setting.app (fun x -> (x + n - 1uy) % n) color
    let bk() = Setting.app (fun x -> (x + 1uy) % n) color
    
    do 
        this
        |* Clickable((fun () -> (if not this.Selected then this.Select()); fd ()), OnHover = fun b -> if b && not this.Focused then this.Focus())
    
    override this.Draw() =
        base.Draw()
        if this.Selected then Draw.rect this.Bounds (!*Palette.SELECTED)
        elif this.Focused then Draw.rect this.Bounds (!*Palette.HOVER)
        Draw.quad (Quad.ofRect this.Bounds) (Quad.colorOf Color.White) (Sprite.gridUV (3, int color.Value) sprite)
    
    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
            
        if this.Selected then
            if (!|"up").Tapped() then fd()
            elif (!|"down").Tapped() then bk()
            elif (!|"left").Tapped() then bk()
            elif (!|"right").Tapped() then fd()

type EditNoteskinPage(refreshNoteskins : unit -> unit) as this =
    inherit Page()

    let data = Noteskins.Current.config
        
    let name = Setting.simple data.Name
    let holdNoteTrim = Setting.bounded data.HoldNoteTrim 0.0f 2.0f |> Setting.roundf 2
    let enableColumnLight = Setting.simple data.EnableColumnLight
    let keycount = Setting.simple options.KeymodePreference.Value
    let mutable noteColors = data.NoteColors
        
    let g keycount i =
        let k = if noteColors.UseGlobalColors then 0 else int keycount - 2
        Setting.make
            (fun v -> noteColors.Colors.[k].[i] <- v)
            (fun () -> noteColors.Colors.[k].[i])

    let colors, refreshColors =
        refreshRow
            (fun () -> ColorScheme.count (int keycount.Value) noteColors.Style)
            (fun i k ->
                let x = -60.0f * float32 k
                let n = float32 i
                NoteColorPicker(g keycount.Value i, Position = { Position.Default with Left = 0.5f %+ (x + 120.0f * n); Right = 0.5f %+ (x + 120.0f * n + 120.0f) })
            )

    do
        this.Content(
            column()
            |+ PrettySetting("themes.editnoteskin.noteskinname", TextEntry(name, "none")).Pos(200.0f)
            |+ PrettySetting("themes.editnoteskin.holdnotetrim", Slider(holdNoteTrim, 0.05f)).Pos(280.0f)
            |+ PrettySetting("themes.editnoteskin.enablecolumnlight", Selector<_>.FromBool enableColumnLight).Pos(360.0f)
            |+ PrettySetting("generic.keymode",
                    Selector<Keymode>.FromEnum(keycount |> Setting.trigger (ignore >> refreshColors))
                ).Pos(460.0f)
            |+ PrettySetting("themes.editnoteskin.globalcolors",
                    Selector<_>.FromBool(
                        Setting.make
                            (fun v -> noteColors <- { noteColors with UseGlobalColors = v })
                            (fun () -> noteColors.UseGlobalColors)
                        |> Setting.trigger (ignore >> refreshColors))
                ).Pos(540.0f)
            |+ PrettySetting("themes.editnoteskin.colorstyle",
                    Selector.FromEnum(
                        Setting.make
                            (fun v -> noteColors <- { noteColors with Style = v })
                            (fun () -> noteColors.Style)
                        |> Setting.trigger (ignore >> refreshColors))
                ).Pos(620.0f)
            |+ PrettySetting("themes.editnoteskin.notecolors", colors).Pos(700.0f, Viewport.vwidth - 200.0f, 120.0f)
        )

    override this.Title = data.Name
    override this.OnClose() =
        Noteskins.Current.changeConfig
            { data with
                Name = name.Value
                HoldNoteTrim = holdNoteTrim.Value
                EnableColumnLight = enableColumnLight.Value
                NoteColors = noteColors
            }
        refreshNoteskins()