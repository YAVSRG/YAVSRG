namespace Interlude.UI.Screens.LevelSelect

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
open Interlude.Gameplay.Collections
open Interlude.Options

module private Collections =

    let private editCollection ((originalName, data): string * Collection) =
        let name = Setting.simple originalName |> Setting.alphaNum
        let originalType = match data with Collection _ -> "Collection" | Playlist _ -> "Playlist" | Goals _ -> "Goals"
        let ctype = Setting.simple originalType
        {
            Content = fun add ->
                column [
                    PrettySetting("collections.edit.collectionname", TextField name).Position(200.0f)
                    PrettySetting("collections.edit.type", Selector([|"Collection", "Collection"; "Playlist", "Playlist"; "Goals", "Goals"|], ctype)).Position(300.0f)
                ] :> Selectable
            Callback = fun () ->
                if name.Value <> originalName then
                    Logging.Debug (sprintf "Renaming collection '%s' to '%s'" originalName name.Value)
                    if (Collections.get name.Value).IsSome then
                        Logging.Debug "Rename failed, target collection already exists."
                        name.Value <- originalName
                    else
                        Collections.rename (originalName, name.Value) |> ignore

                let data =
                    if originalType <> ctype.Value then
                        Logging.Debug (sprintf "Changing type of collection to %s" ctype.Value)
                        match ctype.Value with
                        | "Collection" -> data.ToCollection()
                        | "Playlist" -> data.ToPlaylist(selectedMods.Value, rate.Value)
                        | "Goals" -> data.ToGoals(selectedMods.Value, rate.Value)
                        | _ -> failwith "impossible"
                    else data
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
                        PrettySetting("collections",
                            CardSelect.Selector(
                                setting,
                                { CardSelect.Config.Default with
                                    NameFunc = fst
                                    MarkFunc = (fun (x, m) -> if m then Tree.updateDisplay(); selected <- x)
                                    EditFunc = Some editCollection
                                    CreateFunc = Some (fun () -> Collections.create (Collections.getNewName(), (Collection.Blank)) |> ignore)
                                    DeleteFunc = Some
                                        ( fun (name, data) ->
                                            if fst selected <> name then
                                                if data.IsEmpty() then Collections.delete name |> ignore
                                                else ConfirmDialog(sprintf "Really delete collection '%s'?" name, fun () -> Collections.delete name |> ignore).Show()
                                        )
                                }, add)).Position(200.0f, 1200.0f, 600.0f)
                        TextBox(K <| Localisation.localiseWith [options.Hotkeys.AddToCollection.Value.ToString()] "collections.addhint", K (Color.White, Color.Black), 0.5f)
                        |> positionWidget(0.0f, 0.0f, -190.0f, 1.0f, 0.0f, 1.0f, -120.0f, 1.0f)
                        TextBox(K <| Localisation.localiseWith [options.Hotkeys.RemoveFromCollection.Value.ToString()] "collections.removehint", K (Color.White, Color.Black), 0.5f)
                        |> positionWidget(0.0f, 0.0f, -120.0f, 1.0f, 0.0f, 1.0f, -50.0f, 1.0f)
                    ] :> Selectable
            Callback = ignore
        }
    
type CollectionManager() as this =
    inherit Widget()

    do StylishButton ((fun () -> SelectionMenu(N"collections", Collections.page()).Show()), K "Collections", (fun () -> Style.accentShade(100, 0.6f, 0.4f)), options.Hotkeys.Collections) |> this.Add
    
    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)


        if Chart.cacheInfo.IsSome then

            let selectedChart = Chart.cacheInfo.Value.FilePath

            if options.Hotkeys.AddToCollection.Value.Tapped() && fst selected <> snd contextIndex then
                if
                    match snd selected with
                    | Collection ccs -> if ccs.Contains selectedChart then false else ccs.Add selectedChart; true
                    | Playlist ps -> ps.Add (selectedChart, PlaylistData.Make selectedMods.Value rate.Value); true
                    | Goals gs -> gs.Add (selectedChart, GoalData.Make selectedMods.Value rate.Value Goal.None); true
                then
                    if options.ChartGroupMode.Value = "Collections" then LevelSelect.refresh <- true else Tree.updateDisplay()
                    Notification.add (Localisation.localiseWith [Chart.cacheInfo.Value.Title; fst selected] "collections.added", Info)

            elif options.Hotkeys.RemoveFromCollection.Value.Tapped() then
                if fst selected <> snd contextIndex then // Remove from collection that isn't in this context
                    if
                        match snd selected with
                        | Collection ccs -> ccs.Remove selectedChart
                        | Playlist ps -> 
                            if ps.FindAll(fun (id, _) -> id = selectedChart).Count = 1 then 
                                ps.RemoveAll(fun (id, _) -> id = selectedChart) > 0
                            else false
                        | Goals gs ->
                            if gs.FindAll(fun (id, _) -> id = selectedChart).Count = 1 then 
                                gs.RemoveAll(fun (id, _) -> id = selectedChart) > 0
                            else false
                    then
                        if options.ChartGroupMode.Value = "Collections" then LevelSelect.refresh <- true else Tree.updateDisplay()
                        Notification.add (Localisation.localiseWith [Chart.cacheInfo.Value.Title; fst selected] "collections.removed", Info)
                else // Remove from this context collection
                    if
                        match snd selected with
                        | Collection ccs -> ccs.Remove selectedChart
                        | Playlist ps -> ps.RemoveAt(fst contextIndex); true
                        | Goals gs -> gs.RemoveAt(fst contextIndex); true
                    then
                        LevelSelect.refresh <- true
                        notifyChangeChart LevelSelectContext.None rate selectedMods
                        Notification.add (Localisation.localiseWith [Chart.cacheInfo.Value.Title; fst selected] "collections.removed", Info)

            elif options.Hotkeys.ReorderCollectionDown.Value.Tapped() then
                if reorder false then LevelSelect.refresh <- true

            elif options.Hotkeys.ReorderCollectionUp.Value.Tapped() then
                if reorder true then LevelSelect.refresh <- true