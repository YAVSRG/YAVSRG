namespace Prelude.Data.Options

open System.IO
open System.Collections.Generic
open Percyqaz.Common
open Prelude.Data.ChartDatabase
open Prelude.Gameplay.Rulesets

type RulesetSelection =
    | FileId of string
    | Automatic

type RulesetLibrary(rulesets_storage_path: string) =

    /// Ruleset IDs are the name of the file under the Rulesets folder, without the extension
    /// e.g. Rulesets/my-ruleset.ruleset is loaded with id 'my-ruleset'
    /// Not to be confused with a ruleset 'hash' which uniquely identifies what its actual behaviour is
    /// You could have two different versions of a ruleset with different grade colors
    //   If their gameplay behaviour is the same they get the same hash for gameplay purposes
    //   They get different IDs which allows the user to distinguish and select the version they want
    let DEFAULT_ID = "sc-j4"

    let mutable initialised = false
    let loaded = Dictionary<string, Ruleset>()
    let mutable current_selection = FileId(DEFAULT_ID)
    let on_changed_ev = Event<unit>()
    member val OnChanged = on_changed_ev.Publish

    /// Once called: All rulesets from the Rulesets folder have been read; The valid ones are loaded into the available list
    /// Can be called multiple times to re-load with the latest data
    member this.Load() : unit =
        loaded.Clear()

        let default_path = Path.Combine(rulesets_storage_path, DEFAULT_ID + ".ruleset")

        if not(File.Exists(default_path)) then
            Ruleset.DefaultRuleset.WriteToFile(default_path)

        for f in Directory.GetFiles(rulesets_storage_path) do
            if Path.GetExtension(f).ToLower() = ".ruleset" then
                let id = Path.GetFileNameWithoutExtension(f)

                match Ruleset.TryFromFile(f) with
                | Ok ruleset -> loaded.Add(id, ruleset)
                | Error e -> Logging.Error "Error loading ruleset '%s': %O" id e

        if not(loaded.ContainsKey(DEFAULT_ID)) then
            loaded.Add(DEFAULT_ID, Ruleset.DefaultRuleset)

        match current_selection with
        | FileId file_id when not(loaded.ContainsKey(file_id)) ->
            Logging.Warn "Ruleset '%s' not found, switching to default" file_id
            current_selection <- FileId(DEFAULT_ID)
        | _ -> ()

        initialised <- true

    member this.FindById(file_id: string) : Ruleset = loaded.[file_id]

    member this.FindByHash(hash: string) : Ruleset option =
        loaded.Values |> Seq.tryFind(fun ruleset -> ruleset.CalculateHash() = hash)

    member this.Exists(file_id: string) : bool = loaded.ContainsKey(file_id)

    member val Selected: Setting<RulesetSelection> =
        Setting.make
            (function
                | FileId new_id ->
                    if initialised then
                        if not(loaded.ContainsKey(new_id)) then
                            Logging.Warn "Ruleset '%s' not found, switching to default" new_id
                            current_selection <- FileId(DEFAULT_ID)
                        else
                            current_selection <- FileId(new_id)

                        on_changed_ev.Trigger()
                    else
                        current_selection <- FileId(new_id)
                | Automatic -> current_selection <- Automatic
            )
            (fun () -> current_selection)

    member this.Current: Ruleset = SC_J4 // todo: temp!!!
    member this.CurrentHash: string = SC_J4_HASH // todo: temp!!!
    
    member this.GetCurrentRuleset(chart_meta: ChartMeta, selection: RulesetSelection) : Ruleset =
        match selection with
        | FileId file_id -> loaded.[file_id]
        | Automatic ->
            match Ruleset.GetNativeRuleset(chart_meta.Origins) with
            | Some cached_ruleset -> this.FindByHash(cached_ruleset.Hash) |> Option.defaultValue cached_ruleset.Ruleset
            | None -> SC_J4
            
    member this.GetCurrentRuleset(chart_meta: ChartMeta) : Ruleset = this.GetCurrentRuleset(chart_meta, current_selection)

    member this.Get(selection: RulesetSelection) : Ruleset = // todo: temp!!!
        match selection with
        | FileId file_id -> this.FindById(file_id)
        | Automatic -> SC_J4

    /// Each ruleset and its file ID, sorted alphabetically
    member this.ToList() : (string * Ruleset) seq =
        seq {
            for k in loaded.Keys do
                yield (k, loaded.[k])
        }
        |> Seq.sortBy(fun (_, ruleset) -> ruleset.Name.ToLowerInvariant())

    /// Ruleset is saved as a new (non-conflicting) file and added to the available list
    member this.Install(ruleset: Ruleset) : unit =
        let new_id: string = ruleset.Filename()

        let new_id =
            if this.Exists(new_id) then sprintf "%s_%i" new_id (Timestamp.now()) else new_id

        loaded.Add(new_id, ruleset)
        ruleset.WriteToFile(Path.Combine(rulesets_storage_path, new_id + ".ruleset"))

    member this.Install(path: string) : bool =
        if Path.GetDirectoryName(path) = rulesets_storage_path then
            Logging.Debug("User tried to import a ruleset that was already in the Rulesets folder")
            false
        else
            match Ruleset.TryFromFile(path) with
            | Ok ruleset ->
                this.Install(ruleset)
                true
            | Error reason ->
                Logging.Error "Ruleset import failed: %s" reason
                false

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
            loaded.Add(new_file_id, ruleset)
            ruleset.WriteToFile(Path.Combine(rulesets_storage_path, new_file_id + ".ruleset"))
            let selected_was_changed = current_selection = FileId(existing_file_id)
            
            if selected_was_changed then
                current_selection <- FileId(new_file_id)
                on_changed_ev.Trigger()

    member this.Delete(file_id: string) : bool =
        if file_id <> DEFAULT_ID && this.Exists(file_id) then
            try
                File.Delete(Path.Combine(rulesets_storage_path, file_id + ".ruleset"))
                loaded.Remove(file_id)
            with :? IOException as err ->
                Logging.Error "IO error deleting ruleset: %O" err
                false
        else
            false
