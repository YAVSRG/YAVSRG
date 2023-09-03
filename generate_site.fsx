#r "nuget: FSharp.Formatting,18.1.0"

open System.Text.RegularExpressions
open System.IO
open FSharp.Formatting.Markdown

module MarkdownToHtml =

    let rec private paragraph (p: MarkdownParagraph) =
        match p with
        | Heading (size, body, _) -> sprintf "<h%i class=\"text-30\">%s</h%i>" size (spans body) size
        | Paragraph (body, _) -> sprintf "<p class=\"leading-8 mb-4\">%s</p>" (spans body)
        | Span (body, _) -> spans body
        | ListBlock (kind, items, _) ->
            let content = items |> List.map (fun ps -> sprintf "<li class=\"ml-8 list-disc\">%s</li>" (paragraphs ps)) |> String.concat ""
            sprintf "<ul>%s</ul>" content
        | HorizontalRule (char, _) -> "<hr/>"
        | CodeBlock (code, _, _, language, _, _) -> sprintf "<code>%s</code>" code
        | QuotedBlock (ps, _) -> sprintf "<blockquote>%s<blockquote>" (paragraphs ps)
        | YamlFrontmatter _
        | TableBlock _
        | OutputBlock _
        | OtherBlock _
        | LatexBlock _
        | EmbedParagraphs _
        | InlineHtmlBlock _ -> ""

    and private paragraphs (ps: MarkdownParagraphs) =
        List.map paragraph ps |> String.concat ""

    and private spans (ss: MarkdownSpans) =
        List.map span ss |> String.concat ""

    and private span (s: MarkdownSpan) =
        match s with
        | Literal (text, _) -> text
        | InlineCode (code, _) -> sprintf "<code>%s</code>" code
        | Strong (body, _) -> sprintf "<strong>%s</strong>" (spans body)
        | Emphasis (body, _) -> sprintf "<em>%s</em>" (spans body)
        | DirectLink (body, link, title, _) -> sprintf "<a class=\"underline\" href=\"%s\">%s</a>" link (spans body)
        | DirectImage (body, link, title, _) -> sprintf "<img class=\"feature\" src=\"%s\" alt=\"%s\" />" link (Option.defaultValue "" title)
        | IndirectImage (body, link, key, _) -> ""
        | IndirectLink (body, original, key, _) -> ""
        | AnchorLink (link, _) -> ""
        | HardLineBreak _ -> "<br/>"
        | EmbedSpans (customSpans, _) -> ""
        | LatexDisplayMath (code, _) -> ""
        | LatexInlineMath (code, _) -> ""

    let render_document (md: MarkdownDocument) = paragraphs md.Paragraphs

let template = File.ReadAllText("./site_data/page.html")
let changelog_template = File.ReadAllText("./site_data/changelog.html")

let build_page (file: string) (title: string) (content: string) =
    template
        .Replace("{{title}}", title)
        .Replace("{{content}}", content)
        |> fun t -> File.WriteAllText(file, t)

let build_mpage (file: string) (title: string) (contents: string array) =
    let content =
        contents
        |> Array.map (sprintf "<div class=\"frame text-20 container flex flex-col mx-auto p-4\">%s</div>")
        |> String.concat ""
    changelog_template
        .Replace("{{title}}", title)
        .Replace("{{content}}", content)
        |> fun t -> File.WriteAllText(file, t)

Markdown.Parse(File.ReadAllText("./site_data/terms_of_service.md"))
|> MarkdownToHtml.render_document
|> build_page "./site/terms_of_service.html" "Terms of Service"

Markdown.Parse(File.ReadAllText("./site_data/privacy_policy.md"))
|> MarkdownToHtml.render_document
|> build_page "./site/privacy_policy.html" "Privacy Policy"

let re = Regex("(?=\s[0-9]\.[0-9]+\.[0-9]+.*\s*\=\=\=\=)")

File.ReadAllText("./Interlude/docs/changelog.md")
|> re.Split
|> Array.map (fun s -> s.Trim())
|> Array.except [""]
|> Array.map Markdown.Parse
|> Array.map MarkdownToHtml.render_document
|> build_mpage "./site/interlude/changelog.html" "Changelog"

let title (filename: string) =
    let clean = filename.Replace(".md", "").Replace("_", " ")
    clean.[0].ToString().ToUpper() + clean.Substring(1)

let wiki_folders = 
    Directory.EnumerateDirectories("./Interlude/docs/wiki/")
    |> Seq.map (fun path -> (Path.GetFileName path, Directory.EnumerateFiles path |> Seq.map Path.GetFileName))

let wiki_section (item: string, items: seq<string>) =
    sprintf "<hr/><div class=\"ml-3\"><h2>%s</h2><ul class=\"ml-8 list-disc\">%s</ul></div>"
        item
        (items |> Seq.map (fun s -> sprintf "<li class=\"\"><a href=\"%s\">%s</a></li>" (s.Replace(".md", ".html")) (title s)) |> String.concat "")

let wiki_sidebar_content = wiki_folders |> Seq.map wiki_section |> String.concat ""

let wiki_template = File.ReadAllText("./site_data/wiki.html")
for f, items in wiki_folders do
    for i in items do
        let content = 
            File.ReadAllText("./Interlude/docs/wiki/" + f + "/" + i)
                .Split([|"::::"|], System.StringSplitOptions.TrimEntries)
            |> Array.map (Markdown.Parse >> MarkdownToHtml.render_document)
            |> Array.map (sprintf "<div class=\"frame text-20 container flex flex-col mx-auto p-4\">%s</div>")
            |> String.concat ""
        wiki_template
            .Replace("{{title}}", sprintf "%s - Interlude Wiki" (title i))
            .Replace("{{content}}", content)
            .Replace("{{sidebar}}", wiki_sidebar_content)
        |> fun t -> File.WriteAllText("./site/interlude/wiki/" + i.Replace(".md", ".html"), t)

let content = 
    File.ReadAllText("./Interlude/docs/wiki/index.md")
        .Split([|"::::"|], System.StringSplitOptions.TrimEntries)
    |> Array.map (Markdown.Parse >> MarkdownToHtml.render_document)
    |> Array.map (sprintf "<div class=\"frame text-20 container flex flex-col mx-auto p-4\">%s</div>")
    |> String.concat ""
wiki_template
    .Replace("{{title}}", "Interlude Wiki")
    .Replace("{{content}}", content)
    .Replace("{{sidebar}}", wiki_sidebar_content)
|> fun t -> File.WriteAllText("./site/interlude/wiki/index.html", t)