namespace Interlude.Features.Skins.EditHUD

open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Prelude
open Prelude.Skins.HudLayouts
open Interlude.UI
open Interlude.Features.Play

type PositionerContext =
    {
        Screen: Container
        Playfield: Playfield
        State: PlayState
        mutable Selected: HudElement option
        mutable Positioners: Map<HudElement, Widget>
        CreatePositioner: HudElement * PositionerContext -> Widget
        mutable UndoHistory: List<HudElement * HudPosition>
        OnElementMoved: Event<unit>
        HotReload: unit -> unit
        mutable HudContext: HudContext
    }

    member this.Recreate(element: HudElement) : unit =
        match this.Positioners.TryFind(element) with
        | Some existing -> (this.Playfield.Remove(existing) || this.Screen.Remove(existing)) |> ignore
        | None -> ()

        if element.EnabledSetting().Value then

            let p = this.CreatePositioner(element, this)
            let pos = element.PositionSetting().Value

            p.Position <-
                {
                    Left = pos.Left
                    Top = pos.Top
                    Right = pos.Right
                    Bottom = pos.Bottom
                }

            if pos.RelativeToPlayfield then this.Playfield.Add(p) else this.Screen.Add(p)

            this.Positioners <- this.Positioners.Add(element, p)

    member this.CreateAll() : unit =
        for element in HudElement.DRAW_ORDER do
            this.Recreate(element)

    member this.Select(element: HudElement) : unit = this.Selected <- Some element
    member this.ClearSelection() : unit = this.Selected <- None

    member this.ChangeCurrentAnchor(to_playfield: bool, anchor: float32) : unit =
        match this.Selected |> Option.bind this.Positioners.TryFind with
        | Some p ->
            let setting = this.Selected.Value.PositionSetting()
            let current = setting.Value

            this.AddUndoHistory(this.Selected.Value, current)

            let bounds = p.Bounds

            let parent_bounds =
                if to_playfield then this.Playfield.Bounds else this.Screen.Bounds

            let axis = parent_bounds.Left + parent_bounds.Width * anchor

            setting.Set(
                {
                    RelativeToPlayfield = to_playfield
                    Left = anchor %+ (bounds.Left - axis)
                    Top = current.Top
                    Right = anchor %+ (bounds.Right - axis)
                    Bottom = current.Bottom
                }
            )

            this.Recreate(this.Selected.Value)
        | None -> ()

    member this.ResetCurrentPosition() : unit =
        match this.Selected with
        | Some element ->
            let setting = this.Selected.Value.PositionSetting()
            this.AddUndoHistory(element, setting.Value)
            setting.Set(element.DefaultPosition())
            GameThread.defer(fun () -> this.Recreate(element))
        | None -> ()

    member this.AddUndoHistory(element: HudElement, position: HudPosition) : unit =
        this.UndoHistory <- (element, position) :: this.UndoHistory

    member this.Undo() : unit =
        match this.UndoHistory with
        | (element, previous_position) :: xs ->
            this.Selected <- Some element
            element.PositionSetting().Set(previous_position)
            this.Recreate(element)
            Style.click.Play()
            this.UndoHistory <- xs
        | _ -> ()

    member this.ConfigureElement() : unit =
        match this.Selected with
        | Some element ->
            element.ShowMenu(fun () ->
                this.HudContext <- { this.HudContext with Config = Interlude.Content.Content.HUD }
                GameThread.defer(fun () -> this.Recreate(element))
            )
        | None -> ()

    member this.RemoveElement() : unit =
        match this.Selected with
        | Some element ->
            element.EnabledSetting().Set(false)
            this.Recreate(element)
            this.Selected <- None
            Style.click.Play()
        | None -> ()

    member this.HorizontalFlip() : unit =
        match this.Selected with
        | Some element ->
            element.PositionSetting() |> Setting.app _.FlipHorizontal
            this.Recreate(element)
        | None -> ()

    member this.VerticalFlip() : unit =
        match this.Selected with
        | Some element ->
            element.PositionSetting() |> Setting.app _.FlipVertical
            this.Recreate(element)
        | None -> ()

    member this.HorizontalFlipAll() : unit =
        for element in HudElement.DRAW_ORDER do
            element.PositionSetting() |> Setting.app _.FlipHorizontal
            this.Recreate(element)

        Notifications.Result(
            Icons.REPEAT,
            %"hud.horiontal_flip_all.success",
            [ (%%"hud_flip_horizontal_all").ToString() ] %> "hud.horizontal_flip_all.undo_hint"
        )

    member this.VerticalFlipAll() : unit =
        for element in HudElement.DRAW_ORDER do
            element.PositionSetting() |> Setting.app _.FlipVertical
            this.Recreate(element)

        Notifications.Result(
            Icons.REPEAT,
            %"hud.vertical_flip_all.success",
            [ (%%"hud_flip_horizontal_all").ToString() ] %> "hud.vertical_flip_all.undo_hint"
        )
