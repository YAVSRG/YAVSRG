namespace Interlude.UI.Screens.LevelSelect

open System.Drawing
open Prelude.Common
open Prelude.Data.Charts.Library
open Prelude.Data.Charts.Collections
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Components
open Interlude.UI.Components.Selection
open Interlude.UI.Components.Selection.Controls
open Interlude.UI.Components.Selection.Menu
open Interlude.UI.Components.Selection.Compound
open Interlude.Gameplay
open Interlude.Options
open Globals

module private Collections =

    let mutable selected =
        //todo: load from settings
        let favourites = Localisation.localise "collections.Favourites"
        let c = 
            match Collections.get favourites with
            | Some c -> c
            | None ->
                let n = Collection.Blank
                Collections.create (favourites, n) |> ignore
                n
        favourites, c

    let private editCollection ((originalName, data): string * Collection) =
        let name = Setting.simple originalName |> Setting.alphaNum
        {
            Content = fun add ->
                column [
                    PrettySetting("CollectionName", TextField name).Position(200.0f)
                ] :> Selectable
            Callback = fun () ->
                if name.Value <> originalName then
                    Logging.Debug (sprintf "Renaming collection '%s' to '%s'" originalName name.Value)
                    if (Collections.get name.Value).IsSome then
                        Logging.Debug "Rename failed, target collection already exists."
                        name.Value <- originalName
                    else
                        Collections.rename (originalName, name.Value) |> ignore
                Collections.update (name.Value, data)
        }

    let page() =
        {
            Content = fun add ->
                let setting =
                    Setting.make ignore
                        (fun () -> Collections.enumerate() |> Seq.map (fun n -> (n, n |> Collections.get |> Option.get), fst selected = n))
                column
                    [
                        PrettySetting("Collections",
                            CardSelect.Selector(
                                setting,
                                { CardSelect.Config.Default with
                                    NameFunc = fst
                                    MarkFunc = (fun (x, m) -> if m then colorVersionGlobal <- colorVersionGlobal + 1; selected <- x)
                                    EditFunc = Some editCollection
                                    CreateFunc = Some (fun () -> Collections.create (Collections.getNewName(), (Collection.Blank)) |> ignore)
                                    DeleteFunc = Some (fun (name, _) -> if fst selected <> name then Collections.delete name |> ignore)
                                }, add)).Position(200.0f, 1200.0f, 600.0f)
                        TextBox(K <| Localisation.localiseWith [options.Hotkeys.AddToCollection.Value.ToString()] "collections.AddHint", K (Color.White, Color.Black), 0.5f)
                        |> positionWidget(0.0f, 0.0f, -190.0f, 1.0f, 0.0f, 1.0f, -120.0f, 1.0f)
                        TextBox(K <| Localisation.localiseWith [options.Hotkeys.RemoveFromCollection.Value.ToString()] "collections.RemoveHint", K (Color.White, Color.Black), 0.5f)
                        |> positionWidget(0.0f, 0.0f, -120.0f, 1.0f, 0.0f, 1.0f, -50.0f, 1.0f)
                    ] :> Selectable
            Callback = ignore
        }
    
type CollectionManager() =
    inherit Widget()
    
    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        if options.Hotkeys.Collections.Value.Tapped() then
            Dialog.add <| SelectionMenu(Collections.page())
        if currentCachedChart.IsSome then
            if options.Hotkeys.AddToCollection.Value.Tapped() then
                if
                    match snd Collections.selected with
                    | Collection ccs -> if ccs.Contains selectedChart then false else ccs.Add selectedChart; true
                    | Playlist ps -> ps.Add (selectedChart, selectedMods, rate); true
                    | Goals gs -> false //gs.Add ((selectedChart, selectedMods, rate), Goal.NoGoal); true
                then
                    colorVersionGlobal <- colorVersionGlobal + 1
                    Notification.add (Localisation.localiseWith [currentCachedChart.Value.Title; fst Collections.selected] "collections.Added", NotificationType.Info)
            elif options.Hotkeys.RemoveFromCollection.Value.Tapped() then
                if
                    match snd Collections.selected with
                    | Collection ccs -> ccs.Remove selectedChart
                    | Playlist ps -> ps.RemoveAll(fun (id, _, _) -> id = selectedChart) > 0
                    | Goals gs -> gs.RemoveAll(fun ((id, _, _), _) -> id = selectedChart) > 0
                then
                    colorVersionGlobal <- colorVersionGlobal + 1
                    Notification.add (Localisation.localiseWith [currentCachedChart.Value.Title; fst Collections.selected] "collections.Removed", NotificationType.Info)