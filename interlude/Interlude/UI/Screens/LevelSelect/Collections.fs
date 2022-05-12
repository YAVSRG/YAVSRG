namespace Interlude.UI.Screens.LevelSelect

open Prelude.Common
open Prelude.Data.Charts.Caching
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

module CollectionManager =

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
                        (fun () -> Collections.enumerate() |> Seq.map (fun n -> (n, n |> Collections.get |> Option.get), selectedName = n))
                column
                    [
                        PrettySetting("collections",
                            CardSelect.Selector(
                                setting,
                                { CardSelect.Config.Default with
                                    NameFunc = fst
                                    MarkFunc = (fun (x, m) -> if m then LevelSelect.minorRefresh <- true; select(fst x))
                                    EditFunc = Some editCollection
                                    CreateFunc = Some (fun () -> Collections.create (Collections.getNewName(), (Collection.Blank)) |> ignore)
                                    DeleteFunc = Some
                                        ( fun (name, data) ->
                                            if selectedName <> name then
                                                if data.IsEmpty() then Collections.delete name |> ignore
                                                else ConfirmDialog(sprintf "Really delete collection '%s'?" name, fun () -> Collections.delete name |> ignore).Show()
                                        )
                                }, add)).Position(200.0f, 1200.0f, 600.0f)
                        TextBox(K <| Localisation.localiseWith [(!|Hotkey.AddToCollection).ToString()] "collections.addhint", K (Color.White, Color.Black), 0.5f)
                            .Position { Left = 0.0f %+ 0.0f; Top = 1.0f %+ -190.0f; Right = 1.0f %+ 0.0f; Bottom = 1.0f %+ -120.0f }
                        TextBox(K <| Localisation.localiseWith [(!|Hotkey.RemoveFromCollection).ToString()] "collections.removehint", K (Color.White, Color.Black), 0.5f)
                            .Position { Left = 0.0f %+ 0.0f; Top = 1.0f %+ -120.0f; Right = 1.0f %+ 0.0f; Bottom = 1.0f %+ -50.0f }
                    ] :> Selectable
            Callback = ignore
        }

    let addChart(cc: CachedChart, context: LevelSelectContext) =
        if selectedName <> context.InCollection then
            let success = addChart(cc, rate.Value, selectedMods.Value)
            if success then
                if options.ChartGroupMode.Value = "Collections" then LevelSelect.refresh <- true else LevelSelect.minorRefresh <- true
                Notification.add (Localisation.localiseWith [Chart.cacheInfo.Value.Title; selectedName] "collections.added", NotificationType.Info)

    let removeChart(cc: CachedChart, context: LevelSelectContext) =
        let success = removeChart(cc, context)
        if success then
            if options.ChartGroupMode.Value = "Collections" then LevelSelect.refresh <- true else LevelSelect.minorRefresh <- true
            Notification.add (Localisation.localiseWith [Chart.cacheInfo.Value.Title; selectedName] "collections.removed", NotificationType.Info)
            if context = Chart.context then Chart.context <- LevelSelectContext.None

    let dropdownMenuOptions(cc: CachedChart, context: LevelSelectContext) =
        let canRemove =
            context.InCollection = selectedName ||
            match selectedCollection with
            | Collection ccs -> ccs.Contains cc.FilePath
            | Playlist ps -> ps.FindAll(fun (id, _) -> id = cc.FilePath).Count = 1
            | Goals gs -> gs.FindAll(fun (id, _) -> id = cc.FilePath).Count = 1
        [
            if not canRemove then sprintf "%s Add to '%s'" Icons.add selectedName, fun () -> addChart(cc, context)
            else sprintf "%s Remove from '%s'" Icons.remove selectedName, fun () -> removeChart(cc, context)
        ]
    
type CollectionManager() as this =
    inherit Widget()

    do StylishButton ((fun () -> SelectionMenu(N"collections", CollectionManager.page()).Show()), K "Collections", (fun () -> Style.accentShade(100, 0.6f, 0.4f)), Hotkey.Collections) |> this.Add
    
    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)

        if Chart.cacheInfo.IsSome then

            if (!|Hotkey.AddToCollection).Tapped() then CollectionManager.addChart(Chart.cacheInfo.Value, Chart.context)
            elif (!|Hotkey.RemoveFromCollection).Tapped() then CollectionManager.removeChart(Chart.cacheInfo.Value, Chart.context)
            elif (!|Hotkey.MoveDownInCollection).Tapped() then
                if reorder false then LevelSelect.refresh <- true
            elif (!|Hotkey.MoveUpInCollection).Tapped() then
                if reorder true then LevelSelect.refresh <- true