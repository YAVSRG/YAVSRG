 namespace YAVSRG.CLI
 
 open Percyqaz.Shell
 open YAVSRG.CLI.Utils
 open YAVSRG.CLI.Features

 module Commands =

    let release_all_in_one () =
        Version.publish()
        Wiki.generate_toc()
        Site.generate_site()
        exec "git" "add ../interlude/docs"
        exec "git" "add ../site"
        exec "git" "commit -m \"📘 Update site changelog & wiki\""
        exec "git" "push"

    let register (ctx: ShellContext) =
        ctx            
            .WithCommand("locale_check", "Check locale for mistakes", (fun () -> Check.locale_check "en_GB" false))
            .WithCommand("locale_fix", "Tool to automatically add locale keys", (fun () -> Check.locale_check "en_GB" true))
            .WithCommand("locale_rename", "Tool to rename locale keys/namespaces", "replaced_key", (fun arg -> Check.locale_rename "en_GB" arg))
            .WithCommand("check_linecounts", "Check for particularly large source code files", (fun () -> Check.check_linecounts()))
            .WithCommand("format", "Format all source code files with Fantomas", (fun () -> Check.format_all_code()))

            .WithCommand("version", "Displays the current version of Interlude", Version.version)
            .WithCommand("publish_version", "Publishes a new version of Interlude", Version.publish)
            .WithCommand("release_aio", "All-in-one release script", release_all_in_one)

            .WithCommand("pack_win_x64", "Build an Interlude release and zip it user-ready", Releases.build_win_x64)
            .WithCommand("pack_osx_arm64", "Build an Interlude release and zip it user-ready", Releases.build_osx_arm64)
            .WithCommand("pack_linux_x64", "Build an Interlude release and zip it user-ready", Releases.build_linux_x64)

            .WithCommand("bundle_assets", "Bundle all assets for build pipeline", Assets.bundle_assets)

            .WithCommand("generate_wiki_toc", "Generate markdown wiki table of contents", Wiki.generate_toc)
            .WithCommand("generate_site", "Generate site and wiki pages", Site.generate_site)

            .WithCommand("server_gen_cert", "Generate certificate for local server testing", Server.generate_certs)
            .WithCommand("server_run", "Runs the game server locally in docker", Server.run_in_docker)
            .WithCommand("server_test_domain", "Generate markdown wiki table of contents", Server.run_domain_tests)
            .WithCommand("server_test_full", "Run entire suite of tests against server while it's running", Server.run_all_tests)
            .WithCommand("down_detector", "Ping several services Interlude depends on for status", Server.down_detector)

            .WithCommand("noteskins_update", "Update the noteskins repo based on noteskins in the folder", Noteskins.generate_index)

            .WithCommand("debug_run", "Build and run the game in debug mode", Play.debug_run)
            .WithCommand("update", "Update local git repo to the latest stable tagged release", Play.update)
            
            .WithCommand("exit", "Closes the YAVSRG command line", fun () -> System.Environment.Exit(0))
