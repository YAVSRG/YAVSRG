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
            let content = items |> List.map (fun ps -> sprintf "<li class=\"ml-4 list-disc\">%s</li>" (paragraphs ps)) |> String.concat ""
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
        | DirectImage (body, link, title, _) -> sprintf "<img src=\"%s\" alt=\"%s\">%s</img>" link (Option.defaultValue "" title) body
        | IndirectImage (body, link, key, _) -> ""
        | IndirectLink (body, original, key, _) -> ""
        | AnchorLink (link, _) -> ""
        | HardLineBreak _ -> "<br/>"
        | EmbedSpans (customSpans, _) -> ""
        | LatexDisplayMath (code, _) -> ""
        | LatexInlineMath (code, _) -> ""

    let render_document (md: MarkdownDocument) = paragraphs md.Paragraphs

let template = File.ReadAllText("./site_data/page.html")
let template_m = File.ReadAllText("./site_data/page_m.html")

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
    template_m
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
|> build_mpage "./site/changelog.html" "Changelog"