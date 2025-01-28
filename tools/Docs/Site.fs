namespace YAVSRG.CLI.Features

open System.Text.RegularExpressions
open System.IO
open Percyqaz.Data.Markdown
open YAVSRG.CLI.Utils

module Site =

    module MarkdownToHtml =

        let rec private paragraph (p: MarkdownParagraph) =
            match p with
            | Heading(size, body, _) -> sprintf "<h%i class=\"text-3xl\">%s</h%i>" size (spans body) size
            | Paragraph(body, _) -> sprintf "<p class=\"leading-8 mb-4\">%s</p>" (spans body)
            | Span(body, _) -> spans body
            | ListBlock(kind, items, _) ->
                let content =
                    items
                    |> List.map (fun ps -> sprintf "<li class=\"ml-8 list-disc\">%s</li>" (paragraphs ps))
                    |> String.concat ""

                sprintf "<ul>%s</ul>" content
            | HorizontalRule(char, _) -> "<hr/>"
            | CodeBlock(code, _, _, language, _, _) -> sprintf "<code>%s</code>" code
            | QuotedBlock(ps, _) -> sprintf "<blockquote>%s<blockquote>" (paragraphs ps)
            | YamlFrontmatter _
            | TableBlock _
            | OutputBlock _
            | OtherBlock _
            | LatexBlock _
            | EmbedParagraphs _
            | InlineHtmlBlock _ -> ""

        and private paragraphs (ps: MarkdownParagraphs) =
            List.map paragraph ps |> String.concat ""

        and private spans (ss: MarkdownSpans) = List.map span ss |> String.concat ""

        and private span (s: MarkdownSpan) =
            match s with
            | Literal(text, _) -> text
            | InlineCode(code, _) -> sprintf "<code>%s</code>" code
            | Strong(body, _) -> sprintf "<strong>%s</strong>" (spans body)
            | Emphasis(body, _) -> sprintf "<em>%s</em>" (spans body)
            | DirectLink(body, link, title, _) -> sprintf "<a class=\"underline\" href=\"%s\">%s</a>" link (spans body)
            | DirectImage(body, link, title, _) ->
                sprintf "<img class=\"feature\" src=\"%s\" alt=\"%s\" />" link (Option.defaultValue "" title)
            | IndirectImage(body, link, key, _) -> ""
            | IndirectLink(body, original, key, _) -> ""
            | AnchorLink(link, _) -> ""
            | HardLineBreak _ -> "<br/>"
            | EmbedSpans(customSpans, _) -> ""
            | LatexDisplayMath(code, _) -> ""
            | LatexInlineMath(code, _) -> ""

        let render_document (md: MarkdownDocument) = paragraphs md.Paragraphs

    //// WIKI GENERATOR

    type private WikiPage =
        {
            Title: string
            Folder: string
            Html: string
            Filename: string
        }

    let private parse_wiki_file (path: string) =
        let text = File.ReadAllText(path).Replace("\r", "")
        let split = text.Split("---", 3, System.StringSplitOptions.TrimEntries)

        if split.Length <> 3 || split.[0] <> "" then
            failwithf "Problem with format of wiki file: %s" path

        let header_info =
            try
                let header =
                    split.[1].Split("\n")
                    |> Array.map (fun line ->
                        let parts = line.Split(":", System.StringSplitOptions.TrimEntries) in (parts.[0], parts.[1])
                    )
                    |> Map.ofSeq

                if not (header.ContainsKey "title") then
                    failwith "Page is missing 'title'"

                if not (header.ContainsKey "folder") then
                    failwith "Page is missing 'folder'"

                header
            with err ->
                failwithf "Problem parsing header of file: %s (%O)" path err

        let html =
            split.[2].Split("::::", System.StringSplitOptions.TrimEntries)
            |> Array.map (Markdown.Parse >> MarkdownToHtml.render_document)
            |> Array.map (sprintf "<div class=\"frame text-2xl container flex flex-col mx-auto p-4\">%s</div>")
            |> String.concat ""

        {
            Title = header_info.["title"]
            Folder = header_info.["folder"]
            Html = html
            Filename = Path.GetFileNameWithoutExtension(path)
        }

    let generate_index (version: string) =
        let today = System.DateTime.UtcNow.ToString("dd-MM-yyyy")

        let template = File.ReadAllText(Path.Combine(SITE_PATH, "templates", "index.html"))

        template
            .Replace("{{latest_release}}", version)
            .Replace("{{latest_release_date}}", today)
        |> fun t -> File.WriteAllText((Path.Combine(SITE_PATH, "files", "index.html")), t)

    let generate_site () =

        let template = File.ReadAllText(Path.Combine(SITE_PATH, "templates", "page.html"))

        let changelog_template =
            File.ReadAllText(Path.Combine(SITE_PATH, "templates", "changelog.html"))

        let build_page (file: string) (title: string) (content: string) =
            template.Replace("{{title}}", title).Replace("{{content}}", content)
            |> fun t -> File.WriteAllText(file, t)

        let build_mpage (file: string) (title: string) (contents: string array) =
            let content =
                contents
                |> Array.map (sprintf "<div class=\"frame text-2xl container flex flex-col mx-auto p-4\">%s</div>")
                |> String.concat ""

            changelog_template.Replace("{{title}}", title).Replace("{{content}}", content)
            |> fun t -> File.WriteAllText(file, t)

        Markdown.Parse(File.ReadAllText(Path.Combine(SITE_PATH, "templates", "terms_of_service.md")))
        |> MarkdownToHtml.render_document
        |> build_page (Path.Combine(SITE_PATH, "files", "terms_of_service.html")) "Terms of Service"

        Markdown.Parse(File.ReadAllText(Path.Combine(SITE_PATH, "templates", "privacy_policy.md")))
        |> MarkdownToHtml.render_document
        |> build_page (Path.Combine(SITE_PATH, "files", "privacy_policy.html")) "Privacy Policy"

        let re = Regex("(?=\s[0-9]\.[0-9]+\.[0-9]+.*\s*\=\=\=\=)")

        File.ReadAllText(Path.Combine(YAVSRG_PATH, "interlude", "docs", "changelog.md"))
        |> re.Split
        |> Array.map (fun s -> s.Trim())
        |> Array.except [ "" ]
        |> Array.map Markdown.Parse
        |> Array.map MarkdownToHtml.render_document
        |> build_mpage (Path.Combine(SITE_PATH, "files", "interlude", "changelog.html")) "Changelog"

        let wiki_pages =
            Directory.EnumerateFiles(Path.Combine(YAVSRG_PATH, "interlude", "docs", "wiki"))
            |> Seq.filter (fun f -> f.EndsWith ".md" && not (f.EndsWith "index.md"))
            |> Seq.map parse_wiki_file
            |> Array.ofSeq

        let wiki_section (folder: string, pages: seq<WikiPage>) =
            sprintf
                "<hr/><div class=\"ml-3\"><h2>%s</h2><ul class=\"ml-8 list-disc\">%s</ul></div>"
                folder
                (pages
                 |> Seq.map (fun page ->
                     sprintf "<li class=\"\"><a href=\"%s\">%s</a></li>" (page.Filename + ".html") page.Title
                 )
                 |> String.concat "")

        let wiki_sidebar_content =
            wiki_pages
            |> Seq.groupBy (fun p -> p.Folder)
            |> Seq.map wiki_section
            |> String.concat ""
            |> sprintf
                "<div class=\"frame text-2xl container flex flex-col mx-auto p-4\"><h1 class=\"text-3xl\">Table of contents</h1>%s</div>"

        let wiki_template =
            File.ReadAllText(Path.Combine(SITE_PATH, "templates", "wiki.html"))

        for page in wiki_pages do
            wiki_template
                .Replace("{{title}}", sprintf "%s - Interlude Wiki" page.Title)
                .Replace("{{content}}", page.Html)
            |> fun t ->
                File.WriteAllText(Path.Combine(SITE_PATH, "files", "interlude", "wiki", page.Filename + ".html"), t)

        let content =
            File
                .ReadAllText(Path.Combine(YAVSRG_PATH, "interlude", "docs", "wiki", "index.md"))
                .Split([| "::::" |], System.StringSplitOptions.TrimEntries)
            |> Array.map (Markdown.Parse >> MarkdownToHtml.render_document)
            |> Array.map (sprintf "<div class=\"frame text-2xl container flex flex-col mx-auto p-4\">%s</div>")
            |> String.concat ""

        wiki_template
            .Replace("{{title}}", "Interlude Wiki")
            .Replace("{{content}}", content + wiki_sidebar_content)
        |> fun t -> File.WriteAllText(Path.Combine(SITE_PATH, "files", "interlude", "wiki", "index.html"), t)