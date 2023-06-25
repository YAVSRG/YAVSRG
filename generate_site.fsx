#r "nuget: FSharp.Formatting,18.1.0"

open System.IO
open FSharp.Formatting.Markdown

let rec paragraph (p: MarkdownParagraph) =
    match p with
    | Heading (size, body, _) -> sprintf "<h%i class=\"text-30\">%s</h%i>" size (spans body) size
    | Paragraph (body, _) -> sprintf "<p class=\"leading-8 mb-4\">%s</p>" (spans body)
    | Span (body, _) -> spans body
    | ListBlock (kind, items, _) ->
        let content = items |> List.map (fun ps -> sprintf "<li class=\"ml-4 list-disc\">%s</li>" (paragraphs ps)) |> String.concat ""
        sprintf "<ul>%s</ul>" content
    | HorizontalRule (char, _) -> "<hr/>"
    | CodeBlock (code, _, _, language, _, _) -> sprintf "<code>%s</code>" code
    | YamlFrontmatter _
    | TableBlock _
    | OutputBlock _
    | OtherBlock _
    | LatexBlock _
    | QuotedBlock _
    | EmbedParagraphs _
    | InlineHtmlBlock _ -> ""

and paragraphs (ps: MarkdownParagraphs) =
    List.map paragraph ps |> String.concat ""

and spans (ss: MarkdownSpans) =
    List.map span ss |> String.concat ""

and span (s: MarkdownSpan) =
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

let render_document (title: string) (md: MarkdownDocument) =
    sprintf "<!DOCTYPE html><html lang=\"en\"><head><meta charset=\"utf-8\"><title>%s</title>
    <link href=\"https://unpkg.com/tailwindcss@^1.0/dist/tailwind.min.css\" rel=\"stylesheet\">
    <link href=\"style.css\" rel=\"stylesheet\"><link rel=\"icon\" type=\"image/x-icon\" href=\"favicon.ico\">
    </head><body class=\"background-1 akrobat\"><div class=\"flex flex-col h-screen justify-between\">
    <!-- content --><div class=\"py-4 items-center text-center\">
    <div class=\"frame-green text-30 container p-4 mx-auto\">%s</div>
    </div><div class=\"w-full p-10 background-2\"><div class=\"frame text-20 container flex flex-col mx-auto space-y-4 p-4\">
    %s</div></div><!-- footer --><footer class=\"footer text-20 px-4 flex flex-row justify-center\">
    <span>© Percyqaz 2018-2023</span></footer></div></body></html>" title title (paragraphs md.Paragraphs)

Markdown.Parse(File.ReadAllText("./site/terms_of_service.md"))
|> render_document "Terms of Service"
|> fun t -> File.WriteAllText("./site/terms_of_service.html", t)

Markdown.Parse(File.ReadAllText("./site/privacy_policy.md"))
|> render_document "Privacy Policy"
|> fun t -> File.WriteAllText("./site/privacy_policy.html", t)