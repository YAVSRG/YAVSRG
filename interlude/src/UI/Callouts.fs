namespace Interlude.UI

open System
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude.Common
open Interlude.UI

type CalloutContent =
    | Header of string
    | Body of string[]
    | Hotkey of Hotkey

type CalloutLifespan =
    | Notification
    | KeyHeld of Bind

type Callout =
    {
        IsSmall: bool
        Contents: CalloutContent list
        _Icon: string option
    }
    static member Small = { IsSmall = true; Contents = []; _Icon = None }
    static member Normal = { IsSmall = false; Contents = []; _Icon = None }
    member this.Title(s: string) = { this with Contents = Header s :: this.Contents }
    member this.Body(s: string) = { this with Contents = Body (s.Split "\n") :: this.Contents }
    member this.Hotkey(h: Hotkey) = { this with Contents = Hotkey h :: this.Contents }
    member this.Icon(icon: string) = { this with _Icon = Some icon }

module Callout =

    let spacing isSmall = if isSmall then 10.0f else 15.0f
    let header_size isSmall = if isSmall then 25.0f else 35.0f
    let text_size isSmall = if isSmall then 18.0f else 25.0f
    let text_spacing isSmall = if isSmall then 8.0f else 10.0f

    let measure (c: Callout) : float32 =
        let spacing = spacing c.IsSmall
        let mutable result = 0.0f
        for b in c.Contents do
            match b with
            | Header _ -> result <- result + header_size c.IsSmall
            | Body xs -> 
                result <- result + (float32 xs.Length * text_size c.IsSmall) + (float32 (xs.Length - 1) * text_spacing c.IsSmall)
            | Hotkey _ -> result <- result + text_size c.IsSmall
            result <- result + spacing
        result

    let draw (x, y, height, dark, c: Callout) =
        let col = if dark then (Colors.grey1, Colors.white) else (Colors.white, Colors.black)
        let x =
            match c._Icon with
            | Some i ->
                let icon_size = min (if c.IsSmall then 30.0f else 50.0f) height
                Text.drawB(Style.baseFont, i, icon_size, x + 30.0f, y + height * 0.5f - icon_size * 0.7f, col)
                x + icon_size + 60.0f
            | None -> x
        let spacing = spacing c.IsSmall
        let mutable y = y
        for b in c.Contents do
            match b with
            | Header s ->
                let size = header_size c.IsSmall
                Text.drawB(Style.baseFont, s, size, x, y, col)
                y <- y + size
            | Body xs ->
                let size = text_size c.IsSmall
                let tspacing = text_spacing c.IsSmall
                for line in xs do
                    Text.drawB(Style.baseFont, line, size, x, y, col)
                    y <- y + size
                    y <- y + tspacing
                y <- y - tspacing
            | Hotkey hk ->
                let size = text_size c.IsSmall
                // todo: color
                Text.drawB(
                    Style.baseFont,
                    Localisation.localiseWith [(!|hk).ToString()] "misc.hotkeyhint",
                    size, x, y,
                    (Color.FromArgb(0, 120, 190), snd col))
                y <- y + size
            y <- y + spacing

[<RequireQualifiedAccess>]
type NotificationType =
    | Info
    | Warning
    | Error
    | System
    | Task

type private Notification =
    {
        Type: NotificationType
        Bind: Bind option // hold this to keep the notification visible
        Message: string[]
        HotkeyHint: Hotkey option // hotkey displayed in tooltip (for tooltip purposes)

        Fade: Animation.Fade
        mutable Duration: float
    }

module Notifications =

    let private items = ResizeArray<Notification>()

    let private HEIGHT = 120.0f
    let private TEXTHEIGHT = 42.0f
    let mutable up = false

    type Display() =
        inherit Overlay(NodeType.None)

        override this.Update(elapsedTime, moved) =
            base.Update(elapsedTime, moved)
            for i in items do
                i.Duration <- i.Duration - elapsedTime
                i.Fade.Update elapsedTime |> ignore
                if i.Fade.Target <> 0.0f then
                    if i.Duration <= 0.0 || (i.Bind.IsSome && not (i.Bind.Value.Pressed())) then
                        i.Fade.Target <- 0.0f
                elif i.Fade.Value < 0.01f then sync (fun () -> items.Remove i |> ignore)
            if items.Count = 0 then
                up <- Mouse.y() > this.Bounds.CenterY

        override this.Draw() =
            let height notification = 
                match notification.HotkeyHint with
                | Some _ -> HEIGHT + TEXTHEIGHT * float32 notification.Message.Length + 10.0f
                | None -> HEIGHT + TEXTHEIGHT * float32 (notification.Message.Length - 1)
            let draw notification y h =
                let bounds = this.Bounds.Shrink(100.0f, 0.0f).SliceTop(h).Translate(0.0f, y)
                let c, icon =
                    match notification.Type with
                    | NotificationType.Info -> Color.FromArgb(0, 120, 190), Icons.info
                    | NotificationType.Warning -> Color.FromArgb(180, 150, 0), Icons.alert
                    | NotificationType.Error -> Color.FromArgb(190, 0, 0), Icons.alert
                    | NotificationType.System -> Color.FromArgb(0, 190, 120), Icons.system_notification
                    | NotificationType.Task -> Color.FromArgb(120, 0, 190), Icons.system_notification
                let a = notification.Fade.Alpha
                Draw.rect (bounds.SliceLeft 5.0f) (Color.FromArgb(a, c))
                Draw.rect (bounds.SliceTop 5.0f) (Color.FromArgb(a, c))
                Draw.rect (bounds.SliceRight 5.0f) (Color.FromArgb(a, c))
                Draw.rect (bounds.SliceBottom 5.0f) (Color.FromArgb(a, c))
                
                Draw.rect (bounds.Shrink 5.0f) (Color.FromArgb(a / 4 * 3, Color.Black))
                Draw.rect (bounds.Shrink 5.0f) (Color.FromArgb(a / 2, c))
                
                Text.drawB (Style.baseFont, icon, 50.0f, this.Bounds.Left + 130.0f, y - 1.0f + TEXTHEIGHT * 0.5f * float32 notification.Message.Length, (Color.FromArgb(a, Color.White), Color.FromArgb(a, Color.Black)))
                for line = 0 to notification.Message.Length - 1 do
                    Text.drawB (Style.baseFont, notification.Message.[line], 30.0f, this.Bounds.Left + 235.0f, y + 33.0f + TEXTHEIGHT * float32 line, (Color.FromArgb(a, Color.White), Color.FromArgb(a, Color.Black)))
                match notification.HotkeyHint with
                | Some hk -> 
                    let hint = Localisation.localiseWith [(!|hk).ToString()] "misc.hotkeyhint"
                    Text.drawB (Style.baseFont, hint, 30.0f, this.Bounds.Left + 235.0f, y + 43.0f + TEXTHEIGHT * float32 notification.Message.Length, (Color.FromArgb(a, 100, 200, 255), Color.FromArgb(a, Color.Black)))
                | None -> ()

            if up then
                let mutable y = this.Bounds.Top + 200.0f
                for i in items do
                    let h = height i
                    y <- y + h * i.Fade.Value
                    draw i (y - h) h
            else
                let mutable y = this.Bounds.Bottom - 200.0f
                for i in items do
                    let h = height i
                    y <- y - h * i.Fade.Value
                    draw i y h

    let display = Display()

    let tooltip (b: Bind, str: string, hotkey: Hotkey option) =
        let t: Notification =
            {
                Bind = Some b
                Message = str.Split "\n"
                HotkeyHint = hotkey
                Duration = infinity
                Fade = Animation.Fade(0.0f, Target = 1.0f)
                Type = NotificationType.Info
            }
        sync (fun () -> items.Add t)

    let add (str: string, t: NotificationType) =
        let t: Notification =
            {
                Bind = None
                Message = str.Split "\n"
                HotkeyHint = None
                Duration = 2000.0
                Fade = Animation.Fade(0.0f, Target = 1.0f)
                Type = t
            }
        sync (fun () -> items.Add t)