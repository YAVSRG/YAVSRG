 namespace Interlude.Tools
 
 open Percyqaz.Shell
 open Interlude.Tools.Features

 module Commands =
    let register (ctx: ShellContext) =
        ctx            
            .WithCommand("locale_check", "Check locale for mistakes", (fun () -> Check.locale_check "en_GB" false))
            .WithCommand("locale_fix", "Tool to automatically add locale keys", (fun () -> Check.locale_check "en_GB" true))
            .WithCommand("locale_rename", "Tool to rename locale keys/namespaces", "replaced_key", (fun arg -> Check.locale_rename "en_GB" arg))
            .WithCommand("check_linecounts", "Check for particularly large source code files", (fun () -> Check.check_linecounts()))

            .WithCommand("version", "Displays the current version of Interlude", Version.version)
            .WithCommand("publish_version", "Publishes a new version of Interlude", Version.publish)

            .WithCommand("pack_win_x64", "Build an Interlude release and zip it user-ready", Releases.build_win_x64)
            .WithCommand("pack_osx_arm64", "Build an Interlude release and zip it user-ready", Releases.build_osx_arm64)
            .WithCommand("pack_linux_x64", "Build an Interlude release and zip it user-ready", Releases.build_linux_x64)

            .WithCommand("bundle_assets", "Bundle all assets for build pipeline", Assets.bundle_assets)

            .WithCommand("generate_site", "Generate site and wiki pages", Site.generate_site)