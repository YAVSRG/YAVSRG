namespace Interlude.Tools.Features

open Prelude.Data.Themes
open Interlude.Tools

module Asset_Script =

    open System.IO

    let main() =
        let theme = Theme.FromPath(Path.Combine(Utils.ASSETS_PATH, "default"))
        theme.StitchTexture("sc-grade-base", "Rulesets")
        theme.StitchTexture("sc-grade-lamp-overlay", "Rulesets")
        theme.StitchTexture("sc-grade-overlay", "Rulesets")

        
        let dbar = Noteskin.FromPath(Path.Combine(Utils.ASSETS_PATH, "defaultBar"))
        for t in Storage.noteskinTextures do
            dbar.StitchTexture(t)