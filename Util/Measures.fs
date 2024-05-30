module Util.Measures

open LanguagePrimitives


[<Measure>]
type quanta

[<Measure>]
type pctg // Percentage

[<Measure>]
type k // kilo

[<Measure>]
type day

[<Measure>]
type hour


let float__day(src:float) = src |> FloatWithMeasure<day>