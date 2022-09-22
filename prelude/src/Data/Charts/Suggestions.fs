namespace Prelude.Data.Charts.Recommendations

open Prelude.Common

open Prelude.Data.Charts.Caching
open Prelude.Data.Charts.Library
open Prelude.Data.Charts.Sorting

type RecommendationContext =
    {
        Filter: Filter
        BPM: int
        Physical: float
        Challenge: float
        Duration: Time
    }

module Recommendation =
    
    let get_context(current: CachedChart) =
        ()