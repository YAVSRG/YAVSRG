namespace Backbeat.Features.Archive

open System.Text.RegularExpressions

module Collect =

    // matches lowercase text that could have been transliterated directly from japanese kana
    let romaji_regex =
        Regex("^((ssh|chart_metah|ss|kk|tt|pp|ch|sh|[kstnhfmyrwgzdbpj])?y?[aiuoe]|n|tsu|dzu|\s)*$")

    let simplify_string =
        let regex = Regex("[^\sa-zA-Z0-9_]")
        fun (s: string) -> regex.Replace(s.ToLowerInvariant(), "").Trim().Replace(" ", "")