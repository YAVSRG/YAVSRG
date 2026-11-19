namespace Prelude.Data.Options

open System.IO
open System.Collections.Generic
open Percyqaz.Common
open Percyqaz.Data
open Prelude.Data.ChartDatabase
open Prelude.Gameplay.Rulesets

[<Json.AutoCodec>]
type RulesetSelection =
    | FileId of string
    | OsuAutomatic of default_od: float32

type RulesetLibrary(rulesets_storage_path: string) =

    let mutable initialised = false
    let loaded = Dictionary<string, RulesetInfo>()
    let mutable current_selection = FileId(RulesetLibrary.DEFAULT_ID)
    let on_changed_ev = Event<unit>()

    /// Ruleset IDs are the name of the file under the Rulesets folder, without the extension
    /// e.g. Rulesets/my-ruleset.ruleset is loaded with id 'my-ruleset'
    /// Not to be confused with a ruleset 'hash' which uniquely identifies what its actual behaviour is
    /// You could have two different versions of a ruleset with different grade colors
    //   If their gameplay behaviour is the same they get the same hash for gameplay purposes
    //   They get different IDs which allows the user to distinguish and select the version they want
    let DEFAULT = SC_J4
    static member val DEFAULT_ID = SC_J4.Ruleset.Filename()

    member val OnChanged = on_changed_ev.Publish

    /// Once called: All rulesets from the Rulesets folder have been read; The valid ones are loaded into the available list
    /// Can be called multiple times to re-load with the latest data
    member this.Load() : unit =
        loaded.Clear()

        let default_path =
            Path.Combine(rulesets_storage_path, RulesetLibrary.DEFAULT_ID + ".ruleset")

        if not(File.Exists(default_path)) then
            DEFAULT.Ruleset.WriteToFile(default_path)

        for f in Directory.GetFiles(rulesets_storage_path) do
            if Path.GetExtension(f).ToLower() = ".ruleset" then
                let id = Path.GetFileNameWithoutExtension(f)

                match Ruleset.TryFromFile(f) with
                | Ok ruleset -> loaded.Add(id, ruleset.ToInfo())
                | Error e -> Logging.Error "Error loading ruleset '%s': %O" id e

        if not(loaded.ContainsKey(RulesetLibrary.DEFAULT_ID)) then
            loaded.Add(RulesetLibrary.DEFAULT_ID, DEFAULT)

        match current_selection with
        | FileId file_id when not(loaded.ContainsKey(file_id)) ->
            Logging.Warn "Ruleset '%s' not found, switching to default" file_id
            current_selection <- FileId(RulesetLibrary.DEFAULT_ID)
            on_changed_ev.Trigger()
        | _ -> ()

        initialised <- true

    member this.FindByHash(hash: string) : RulesetInfo option =
        loaded.Values |> Seq.tryFind(fun ruleset -> ruleset.Hash = hash)

    member this.Exists(file_id: string) : bool = loaded.ContainsKey(file_id)

    member val Selected: Setting<RulesetSelection> =
        let on_set (selection: RulesetSelection) : unit =
            match selection with
            | FileId new_id ->
                if initialised then
                    if not(loaded.ContainsKey(new_id)) then
                        Logging.Warn "Ruleset '%s' not found, switching to default" new_id
                        current_selection <- FileId(RulesetLibrary.DEFAULT_ID)
                    else
                        current_selection <- FileId(new_id)

                    on_changed_ev.Trigger()
                else
                    current_selection <- FileId(new_id)
            | OsuAutomatic od ->
                current_selection <- OsuAutomatic od
                on_changed_ev.Trigger()

        Setting.make on_set (fun () -> current_selection)

    member this.GetCurrent(chart_meta: ChartMeta option, selection: RulesetSelection) : RulesetInfo =
        match selection with
        | FileId file_id -> loaded.[file_id]
        | OsuAutomatic od ->
            match chart_meta with
            | None -> NativeRuleset.OsuMania(od).ToInfo()
            | Some chart_meta ->
                match Ruleset.GetNativeOsuRuleset(chart_meta.Origins) with
                | Some native_ruleset ->
                    let ruleset_info = native_ruleset.ToInfo()
                    this.FindByHash(ruleset_info.Hash) |> Option.defaultValue ruleset_info
                | None -> NativeRuleset.OsuMania(od).ToInfo()

    member this.GetCurrent(chart_meta: ChartMeta, selection: RulesetSelection) : RulesetInfo =
        this.GetCurrent(Some chart_meta, selection)

    member this.GetCurrent(chart_meta: ChartMeta option) : RulesetInfo =
        this.GetCurrent(chart_meta, current_selection)

    member this.GetCurrent(chart_meta: ChartMeta) : RulesetInfo = this.GetCurrent(Some chart_meta)

    /// Each ruleset and its file ID, sorted alphabetically
    member this.ToList() : (string * RulesetInfo) seq =
        seq {
            for k in loaded.Keys do
                yield (k, loaded.[k])
        }
        |> Seq.sortBy(fun (_, ruleset_info) -> ruleset_info.Ruleset.Name.ToLowerInvariant())

    /// Ruleset is saved as a new (non-conflicting) file and added to the available list
    member this.Install(ruleset: Ruleset) : string =
        let new_id: string = ruleset.Filename()

        let new_id =
            if this.Exists(new_id) then sprintf "%s_%i" new_id (Timestamp.now()) else new_id

        loaded.Add(new_id, ruleset.ToInfo())
        ruleset.WriteToFile(Path.Combine(rulesets_storage_path, new_id + ".ruleset"))
        new_id

    member this.Install(path: string) : string option =
        if Path.GetDirectoryName(path) = rulesets_storage_path then
            Logging.Debug("User tried to import a ruleset that was already in the Rulesets folder")
            None
        else
            match Ruleset.TryFromFile(path) with
            | Ok ruleset -> Some(this.Install(ruleset))
            | Error reason ->
                Logging.Error "Ruleset import failed: %s" reason
                None

    member this.EnsureInstalledAndSelect(ruleset_info: RulesetInfo) : unit =
        let id =
            match loaded.Keys |> Seq.tryFind(fun id -> loaded.[id].Hash = ruleset_info.Hash) with
            | None -> this.Install(ruleset_info.Ruleset)
            | Some id -> id

        this.Selected.Set(FileId(id))

    /// This may change the file name (and therefore ID) based on the name given in the ruleset
    member this.Update(existing_file_id: string, ruleset: Ruleset) : unit =

        let ensure_filename_updated () =
            let new_id: string = ruleset.Filename()

            let new_id =
                if new_id <> existing_file_id && this.Exists(new_id) then
                    sprintf "%s_%i" new_id (Timestamp.now())
                else
                    new_id

            try
                if new_id <> existing_file_id then
                    File.Move(
                        Path.Combine(rulesets_storage_path, existing_file_id + ".ruleset"),
                        Path.Combine(rulesets_storage_path, new_id + ".ruleset")
                    )

                new_id
            with err ->
                Logging.Error "Failed to move ruleset file from '%s' to '%s': %O" existing_file_id new_id err
                existing_file_id

        let new_file_id = ensure_filename_updated()

        if loaded.Remove(existing_file_id) then
            loaded.Add(new_file_id, ruleset.ToInfo())
            ruleset.WriteToFile(Path.Combine(rulesets_storage_path, new_file_id + ".ruleset"))
            let selected_was_changed = current_selection = FileId(existing_file_id)

            if selected_was_changed then
                current_selection <- FileId(new_file_id)
                on_changed_ev.Trigger()

    member this.Delete(file_id: string) : bool =
        if file_id <> RulesetLibrary.DEFAULT_ID && this.Exists(file_id) then
            try
                if current_selection = FileId(file_id) then
                    current_selection <- FileId(RulesetLibrary.DEFAULT_ID)
                    on_changed_ev.Trigger()

                File.Delete(Path.Combine(rulesets_storage_path, file_id + ".ruleset"))
                loaded.Remove(file_id)
            with :? IOException as err ->
                Logging.Error "IO error deleting ruleset: %O" err
                false
        else
            false

    member this.CanDelete(file_id: string) : bool = file_id <> RulesetLibrary.DEFAULT_ID

    member this.CanEdit(file_id: string) : bool = file_id <> RulesetLibrary.DEFAULT_ID
