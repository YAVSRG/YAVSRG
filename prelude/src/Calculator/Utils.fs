namespace Prelude.Calculator

module Layout =

    let keys_on_left_hand (keymode: int) =
        match keymode with
        | 3 -> 2
        | 4 -> 2
        | 5 -> 3
        | 6 -> 3
        | 7 -> 4
        | 8 -> 4
        | 9 -> 5
        | 10 -> 5
        | _ -> failwithf "Invalid keymode %i" keymode

[<Struct>]
type MovingVariance =
    private {
        mutable Sx: float32
        mutable Sxx: float32
        mutable N: float32
    }

module MovingVariance =

    let empty : MovingVariance =
        {
            Sx = 0.0f
            Sxx = 0.0f
            N = 0.0f
        }

    let add (v: float32) (mv: MovingVariance) : MovingVariance =
        {
            N = mv.N + 1.0f
            Sx = mv.Sx + v
            Sxx = mv.Sxx + v * v
        }

    let remove (v: float32) (mv: MovingVariance) : MovingVariance =
        {
            N = mv.N - 1.0f
            Sx = mv.Sx - v
            Sxx = mv.Sxx - v * v
        }

    let get (mv: MovingVariance) : float32 =
        if mv.N <= 0.0f then 0.0f else

        let mean = mv.Sx / mv.N
        mv.Sxx / mv.N - mean * mean

    let get_relative (mv: MovingVariance) : float32 =
        if mv.N <= 0.0f then 0.0f else

        let mean = mv.Sx / mv.N
        (mv.Sxx / mv.N - mean * mean) / mean