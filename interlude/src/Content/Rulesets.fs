namespace Interlude.Content

open Percyqaz.Common
open Prelude
open Prelude.Gameplay.Rulesets
open Prelude.Data.Options

module Rulesets =

    let rulesets: RulesetLibrary = RulesetLibrary(get_game_folder("Rulesets"))
    let DEFAULT_ID = "sc-j4"
    let on_changed = rulesets.OnChanged
    let load () : unit = rulesets.Load()
    let init () : unit = rulesets.Load()
    let selected_id: Setting<RulesetSelection> = rulesets.Selected
    let by_id (id: string) : Ruleset = rulesets.FindById(id).Ruleset
    let by_hash (hash: string) : CachedRuleset option = rulesets.FindByHash(hash)
    let list () : (string * CachedRuleset) seq = rulesets.ToList()
    let exists (id: string) : bool = rulesets.Exists(id)
    let install (ruleset: Ruleset) : unit = rulesets.Install(ruleset)
    let install_from_file (path: string) : bool = rulesets.Install(path)
    let update (existing_id: string) (ruleset: Ruleset) : unit = rulesets.Update(existing_id, ruleset)
    let delete (id: string) : bool = rulesets.Delete(id)

type Rulesets = class end
