namespace Interlude.Content

open System.IO
open System.Collections.Generic
open Percyqaz.Common
open Prelude
open Prelude.Gameplay
open Prelude.Gameplay.PremadeRulesets

module Rulesets =

    let private DEFAULT_ID = "sc-j4"
    let DEFAULT = SC.create 4
    let DEFAULT_HASH = Ruleset.hash DEFAULT

    let mutable private initialised = false
    let private loaded = Dictionary<string, Ruleset>()
    let mutable current = DEFAULT
    let mutable current_hash = DEFAULT_HASH
    let private _selected_id = Setting.simple DEFAULT_ID

    let load () =
        loaded.Clear()

        let path = get_game_folder "Rulesets"
        let default_path = Path.Combine(path, DEFAULT_ID + ".ruleset")

        if not (File.Exists default_path) then
            JSON.ToFile (default_path, true) DEFAULT

        for f in Directory.GetFiles(path) do
            if Path.GetExtension(f).ToLower() = ".ruleset" then
                let id = Path.GetFileNameWithoutExtension(f)

                match JSON.FromFile<Ruleset>(f) with
                | Ok rs -> loaded.Add(id, rs.Validate)
                | Error e -> Logging.Error(sprintf "Error loading ruleset '%s'" id, e)

        if not (loaded.ContainsKey DEFAULT_ID) then
            loaded.Add(DEFAULT_ID, DEFAULT)

    let init_window () =
        load ()

        if not (loaded.ContainsKey _selected_id.Value) then
            Logging.Warn("Ruleset '" + _selected_id.Value + "' not found, switching to default")
            _selected_id.Value <- DEFAULT_ID

        current <- loaded.[_selected_id.Value]
        current_hash <- Ruleset.hash current
        initialised <- true

    let selected_id =
        Setting.make
            (fun new_id ->
                if initialised then
                    if not (loaded.ContainsKey new_id) then
                        Logging.Warn("Ruleset '" + new_id + "' not found, switching to default")
                        _selected_id.Value <- DEFAULT_ID
                    else
                        _selected_id.Value <- new_id

                    current <- loaded.[_selected_id.Value]
                    current_hash <- Ruleset.hash current
                else
                    _selected_id.Value <- new_id
            )
            (fun () -> _selected_id.Value)

    let by_id (id) = loaded.[id]

    let by_hash (hash) =
        loaded.Values |> Seq.tryFind (fun rs -> Ruleset.hash rs = hash)

    let list () =
        seq {
            for k in loaded.Keys do
                yield (k, loaded.[k])
        }
        |> Seq.sortBy (fun (_, rs) -> rs.Name)

    let exists = loaded.ContainsKey

    let install_or_update (new_id: string) (ruleset: Ruleset) =
        loaded.Remove new_id |> ignore
        loaded.Add(new_id, ruleset)

        if _selected_id.Value = new_id then
            current <- ruleset
            current_hash <- Ruleset.hash current

        JSON.ToFile (Path.Combine(get_game_folder "Rulesets", new_id + ".ruleset"), true) ruleset

    let delete (id: string) =
        if exists id then
            try
                File.Delete(Path.Combine(get_game_folder "Rulesets", id + ".ruleset"))
                loaded.Remove id
            with
            | :? IOException as err ->
                Logging.Error("IO error deleting ruleset", err)
                false
        else false