namespace Interlude.Content

open Percyqaz.Common
open Prelude
open Prelude.Gameplay.Rulesets
open Prelude.Data.Options

module Rulesets =

    let rulesets: RulesetLibrary = RulesetLibrary(get_game_folder("Rulesets"))

    let current () = rulesets.Current
    let current_hash () = rulesets.CurrentHash
    let DEFAULT_ID = "sc-j4"
    let on_changed = rulesets.OnChanged
    let load () : unit = rulesets.Load()
    let init () = rulesets.Load()
    let selected_id: Setting<string> = rulesets.Selected
    let by_id (id: string) : Ruleset = rulesets.FindById(id)
    let by_hash (hash: string) : Ruleset option = rulesets.FindByHash(hash)
    let list () : (string * Ruleset) seq = rulesets.ToList()
    let exists (id: string) : bool = rulesets.Exists(id)
    let install (ruleset: Ruleset) : unit = rulesets.Install(ruleset)
    let install_from_file (path: string) : bool = rulesets.Install(path)
    let update (existing_id: string) (ruleset: Ruleset) : unit = rulesets.Update(existing_id, ruleset)
    let delete (id: string) : bool = rulesets.Delete(id)
