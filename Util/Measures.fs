module Util.Measures

open LanguagePrimitives


[<Measure>]
type quantum

[<Measure>]
type pctg // Percentage

[<Measure>]
type k // kilo

[<Measure>]
type day

[<Measure>]
type hour


let float__day(src:float) = src |> FloatWithMeasure<day>