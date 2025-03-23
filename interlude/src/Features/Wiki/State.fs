namespace Interlude.Features.Wiki

open Percyqaz.Common
open Percyqaz.Data
open Percyqaz.Data.Markdown
open Percyqaz.Flux.Windowing
open Prelude.Data

[<Json.AutoCodec>]
type WikiPage =
    {
        Folder: string
        Title: string
        Filename: string
    }

[<AutoOpen>]
module private WikiState =

    type Resource =
        | WikiIndex
        | WikiPage of WikiPage
        | Changelog

    module Cache =

        let mutable changelog_content: MarkdownDocument array option = None
        let mutable index_content: MarkdownDocument array option = None
        let mutable index_table_of_contents: Map<string, WikiPage list> = Map.empty

        let mutable page_cache_by_filename: Map<string, MarkdownDocument array option> =
            Map.empty

        let mutable pages_by_filename: Map<string, WikiPage> = Map.empty

    let mutable loading = false
    let mutable page_history = [ WikiIndex ]
    let mutable current_page = WikiIndex
    let mutable loaded_content: MarkdownDocument array option = None
    let mutable page_changed: unit -> unit = ignore

    let private page_loader =
        { new Async.Queue<Resource, MarkdownDocument array>() with
            override this.Handle(resource) =
                async {
                    match resource with
                    | WikiIndex ->
                        if Cache.index_content.IsNone then
                            match!
                                WebServices.download_string.RequestAsync(
                                    sprintf "https://raw.%s.com/YAVSRG/YAVSRG/main/interlude/docs/wiki/index.md" "githubusercontent"
                                )
                            with
                            | WebResult.Ok md ->
                                match!
                                    WebServices.download_json_async (
                                        sprintf "https://raw.%s.com/YAVSRG/YAVSRG/main/interlude/docs/wiki/index.json" "githubusercontent"
                                    )
                                with
                                | WebResult.Ok toc ->
                                    Cache.index_table_of_contents <- toc

                                    Cache.page_cache_by_filename <-
                                        toc
                                        |> Map.toSeq
                                        |> Seq.map snd
                                        |> Seq.concat
                                        |> Seq.map (fun p -> p.Filename, None)
                                        |> Map.ofSeq

                                    Cache.pages_by_filename <-
                                        toc
                                        |> Map.toSeq
                                        |> Seq.map snd
                                        |> Seq.concat
                                        |> Seq.map (fun p -> p.Filename, p)
                                        |> Map.ofSeq

                                    Cache.index_content <- Some [| Markdown.Parse md |]
                                | otherwise -> Logging.Debug "Getting wiki index.json failed: %O" otherwise
                            | otherwise -> Logging.Debug "Getting wiki index.md failed: %O" otherwise

                        return Cache.index_content |> Option.defaultValue [||]

                    | WikiPage p ->
                        if Cache.page_cache_by_filename.[p.Filename].IsNone then
                            match!
                                WebServices.download_string.RequestAsync(
                                    sprintf "https://raw.%s.com/YAVSRG/YAVSRG/main/interlude/docs/wiki/%s.md" "githubusercontent" p.Filename
                                )
                            with
                            | WebResult.Ok md ->
                                let sections =
                                    md
                                        .Split("---", 3, System.StringSplitOptions.TrimEntries)
                                        .[2].Split("::::", System.StringSplitOptions.TrimEntries)

                                Cache.page_cache_by_filename <-
                                    Cache.page_cache_by_filename.Add(
                                        p.Filename,
                                        Some(sections |> Array.map Markdown.Parse)
                                    )
                            | otherwise -> Logging.Debug "Getting wiki page '%s' failed: %O" p.Filename otherwise

                        return Cache.page_cache_by_filename.[p.Filename] |> Option.defaultValue [||]

                    | Changelog ->
                        if Cache.changelog_content.IsNone then
                            match!
                                WebServices.download_string.RequestAsync(
                                    sprintf "https://raw.%s.com/YAVSRG/YAVSRG/main/interlude/docs/changelog.md" "githubusercontent"
                                )
                            with
                            | WebResult.Ok md ->
                                let version_split_regex = System.Text.RegularExpressions.Regex(@"\s(?=[\.0-9]+\s+====)")
                                Cache.changelog_content <- version_split_regex.Split(md) |> Array.map Markdown.Parse |> Some
                            | otherwise -> Logging.Debug "Getting changelog failed: %O" otherwise

                        return Cache.changelog_content |> Option.defaultValue [||]
                }
        }

    // todo: reimplement links to specific headers
    //if page.Contains('#') then
    //Heading.scrollTo <- page.Substring(page.LastIndexOf '#' + 1).Replace('-', ' ')
    //page.Substring(0, page.LastIndexOf '#')

    let load_resource (resource: Resource) =
        if not loading then
            loading <- true
            page_history <- resource :: (List.except [ resource ] page_history)
            current_page <- resource

            page_loader.Request(
                resource,
                fun md ->
                    loaded_content <- Some md
                    loading <- false
                    GameThread.defer page_changed
            )

    do
        LinkHandler.add
            {
                Condition =
                    fun url ->
                        Cache.pages_by_filename.ContainsKey(
                            url.Replace(".html", "", System.StringComparison.InvariantCultureIgnoreCase)
                        )
                Action =
                    fun url ->
                        Cache.pages_by_filename.[url.Replace(
                            ".html",
                            "",
                            System.StringComparison.InvariantCultureIgnoreCase
                        )]
                        |> WikiPage
                        |> load_resource
            }