namespace YAVSRG.CLI

open System.IO

module Version =

    let mutable private current_version =
        let file = Path.Combine(INTERLUDE_SOURCE_PATH, "Interlude.fsproj")
        let f = File.ReadAllText(file)

        let i = f.IndexOf("<AssemblyVersion>")
        let j = f.IndexOf("</AssemblyVersion>")

        f.Substring(i, j - i).Substring("<AssemblyVersion>".Length)

    let create_publish_commit () : unit =

        let changelog = Path.Combine(FULL_CHANGELOG_PATH)
        let logtxt = File.ReadAllText(changelog)
        let latest = logtxt.Split(current_version + "\r\n" + "====", 2).[0]

        if latest.Trim() = "" then
            failwithf "No changelog for new version. Create this first"

        let v = latest.Split("====", 2).[0].Trim()
        File.WriteAllText(Path.Combine(LATEST_CHANGELOG_PATH), latest)

        printfn "Version: %s -> %s" current_version v
        printfn "%s" latest
        let file = Path.Combine(INTERLUDE_SOURCE_PATH, "Interlude.fsproj")
        let mutable f = File.ReadAllText(file)

        do
            let i = f.IndexOf("<AssemblyVersion>")
            let j = f.IndexOf("</AssemblyVersion>")

            f <- f.Substring(0, i) + "<AssemblyVersion>" + v + f.Substring(j)

        do
            let i = f.IndexOf("<FileVersion>")
            let j = f.IndexOf("</FileVersion>")

            f <- f.Substring(0, i) + "<FileVersion>" + v + f.Substring(j)

        File.WriteAllText(file, f)

        current_version <- v
        Site.generate_index v

        Shell.Exec("git", sprintf "commit -a -m \"🏷️ release: Version %s\"" v) |> ignore

    let version () = current_version
