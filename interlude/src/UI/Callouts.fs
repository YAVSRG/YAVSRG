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

type Callout =
    {
        IsSmall: bool
        Contents: CalloutContent list
        _Icon: string option
    }
    static member Small = { IsSmall = true; Contents = []; _Icon = None }
    static member Normal = { IsSmall = false; Contents = []; _Icon = None }
    member this.Title(s: string) = if s <> "" then { this with Contents = Header s :: this.Contents } else this
    member this.Body(s: string) = if s <> "" then { this with Contents = Body (s.Split "\n") :: this.Contents } else this
    member this.Hotkey(h: Hotkey) = { this with Contents = Hotkey h :: this.Contents }
    member this.Icon(icon: string) = if icon <> "" then { this with _Icon = Some icon } else { this with _Icon = None }

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

type private Notification =
    {
        Data: Callout
        Height: float32
        Fade: Animation.Fade
        Color: Color
        mutable Duration: float
    }

module Notifications =

    let private items = ResizeArray<Notification>()

    type Display() =
        inherit Overlay(NodeType.None)

        override this.Update(elapsedTime, moved) =
            base.Update(elapsedTime, moved)
            for i in items do
                i.Duration <- i.Duration - elapsedTime
                i.Fade.Update elapsedTime
                if i.Fade.Target <> 0.0f then
                    if i.Duration <= 0.0 then i.Fade.Target <- 0.0f
                elif i.Fade.Value < 0.01f then sync(fun () -> items.Remove i |> ignore)

        override this.Draw() =
            let width = this.Bounds.Width / 3.0f // for now
            let padding = 20.0f
            let mutable y = this.Bounds.Top + 70.0f
            for i in items do
                let bounds = Rect.Box(this.Bounds.Right - width * i.Fade.Value, y, width, i.Height + padding * 2.0f)
                Draw.rect (bounds.Expand(5.0f, 0.0f).SliceLeft(5.0f)) (Color.FromArgb(i.Fade.Alpha, i.Color))
                Draw.rect bounds (Color.FromArgb(i.Fade.Alpha * 2 / 3, i.Color))
                Callout.draw (bounds.Left, bounds.Top + padding, i.Height, false, i.Data)
                y <- y + (i.Height + padding * 2.0f) * i.Fade.Value

    let display = Display()

    let tooltip (b: Bind, str: string, hotkey: Hotkey option) = ()

    let private add (body: Callout, color: Color) =
        let n: Notification =
            {
                Data = body
                Height = Callout.measure body
                Color = color
                Fade = Animation.Fade(0.0f, Target = 1.0f)
                Duration = 2000.0
            }
        if Percyqaz.Flux.Utils.isUiThread() then items.Add n else sync(fun () -> items.Add n)

    let task_feedback(icon: string, title: string, description: string) =
        add (Callout.Small.Icon(icon).Body(description).Title(title), Colors.pink)

    let action_feedback(icon: string, title: string, description: string) =
        add (Callout.Small.Icon(icon).Body(description).Title(title), Colors.blue)

    let system_feedback(icon: string, title: string, description: string) =
        add (Callout.Small.Icon(icon).Body(description).Title(title), Colors.green2)

    let error(title, description) =
        add (Callout.Small.Icon(Icons.alert).Body(description).Title(title), Colors.pink)
        
