module Util.Stat

open System.Collections.Generic

open Util.Math
open Util.Bin
open Util.Json
open Util.Text

type Histogram = {
inf:float
sup:float
mutable max:int
grid:int }

type Stat = {
mean: float
middle: float
var: float
median: float
qinf: float
qsup: float
oinf: float
osup: float
inf: float
sup: float
count: int
histogram: Histogram 
histogramData: int[] }

type SpotInStat = {
deviation: float
mutable spot: float
anchor: float
digit: int
unit: string
stat: Stat }

let HisogramIndexing histogram v =
    let i = (float histogram.grid) * (v - histogram.inf) / (histogram.sup - histogram.inf) |> int
    if i = histogram.grid then
        histogram.grid - 1
    else
        i

let samples__histBarNum samples = 
    let dict = new Dictionary<float,bool>()
    samples
    |> Array.iter(fun v -> dict[v] <- true)

    let num = dict.Count
    if num < 300 then
        1 + num / 10
    else
        100

let sample__histogram (samples:float[]) =
    let inf = samples |> Array.min
    let sup =  samples |> Array.max

    let histogram = {
        inf = inf
        sup = sup
        max = 0
        grid = samples__histBarNum samples  }

    let bars = 
        let res = Array.zeroCreate histogram.grid
        samples
        |> Array.iter(fun v -> 
            let index = HisogramIndexing histogram v
            res[index] <- res[index] + 1)
        res

    histogram.max <- bars |> Array.max

    histogram,bars


let samples__stat (samples:float[]) = 
    let mean,var,middle,min,max = meanVarMiddleRange samples
    let median, qinf, qsup, oinf, osup = median3 samples

    let histogram,bars = sample__histogram samples

    {   mean = mean
        var = var
        middle = middle
        median = median
        qinf = qinf
        qsup = qsup
        oinf = oinf
        osup = osup
        inf = min
        sup = max
        count = samples.Length
        histogram = histogram 
        histogramData = bars }


let spot__SpotInStat (digit,unit) spot samples = 

    let stat = samples__stat samples

    if stat.count > 0 && stat.sup > stat.inf then

        let index = sortIndex samples spot

        {   deviation = 2.0 * (float (index - samples.Length/2))/(float samples.Length) * 100.0
            spot = spot
            anchor = stat.median
            digit = digit
            unit = unit
            stat = stat }
    else
        {   deviation = 0.0
            spot = spot
            anchor = spot
            digit = digit
            unit = unit
            stat = stat }


let Stat__clone stat = 
    {
        mean = stat.mean
        middle = stat.middle
        var = stat.var
        median = stat.median
        qinf = stat.qinf
        qsup = stat.qsup
        oinf = stat.oinf
        osup = stat.osup
        inf = stat.inf
        sup = stat.sup
        count = stat.count
        histogram = {
            inf = stat.histogram.inf
            sup = stat.histogram.sup
            max = stat.histogram.max
            grid = stat.histogram.grid }
        histogramData = stat.histogramData |> Array.map(fun bar -> bar) }


// [Histogram] Structure

let Histogram_empty(): Histogram =
    {
        inf = 0.0
        sup = 0.0
        max = 0
        grid = 0
    }

let Histogram__bin (bb:BytesBuilder) (v:Histogram) =

    float__bin bb v.inf
    float__bin bb v.sup
    int32__bin bb v.max
    int32__bin bb v.grid
    ()

let bin__Histogram (bi:BinIndexed):Histogram =
    let bin,index = bi

    {
        inf = 
            bi
            |> bin__float
        sup = 
            bi
            |> bin__float
        max = 
            bi
            |> bin__int32
        grid = 
            bi
            |> bin__int32
    }

let Histogram__json (v:Histogram) =

    [|  ("inf",float__json v.inf)
        ("sup",float__json v.sup)
        ("max",int32__json v.max)
        ("grid",int32__json v.grid)
         |]
    |> Json.Braket

let Histogram__jsonTbw (w:TextBlockWriter) (v:Histogram) =
    json__str w (Histogram__json v)

let Histogram__jsonStr (v:Histogram) =
    (Histogram__json v) |> json__strFinal


let json__Histogramo (json:Json):Histogram option =
    let fields = json |> json__items

    let mutable passOptions = true

    let info =
        match json__tryFindByName json "inf" with
        | None ->
            passOptions <- false
            None
        | Some v -> 
            match v |> json__floato with
            | Some res -> Some res
            | None ->
                passOptions <- false
                None

    let supo =
        match json__tryFindByName json "sup" with
        | None ->
            passOptions <- false
            None
        | Some v -> 
            match v |> json__floato with
            | Some res -> Some res
            | None ->
                passOptions <- false
                None

    let maxo =
        match json__tryFindByName json "max" with
        | None ->
            passOptions <- false
            None
        | Some v -> 
            match v |> json__int32o with
            | Some res -> Some res
            | None ->
                passOptions <- false
                None

    let grido =
        match json__tryFindByName json "grid" with
        | None ->
            passOptions <- false
            None
        | Some v -> 
            match v |> json__int32o with
            | Some res -> Some res
            | None ->
                passOptions <- false
                None

    if passOptions then
        ({
            inf = info.Value
            sup = supo.Value
            max = maxo.Value
            grid = grido.Value }:Histogram) |> Some
    else
        None

let Histogram_clone src =
    let bb = new BytesBuilder()
    Histogram__bin bb src
    bin__Histogram (bb.bytes(),ref 0)

// [Stat] Structure

let Stat_empty(): Stat =
    {
        mean = 0.0
        middle = 0.0
        var = 0.0
        median = 0.0
        qinf = 0.0
        qsup = 0.0
        oinf = 0.0
        osup = 0.0
        inf = 0.0
        sup = 0.0
        count = 0
        histogram = Histogram_empty()
        histogramData = [| |]
    }

let Stat__bin (bb:BytesBuilder) (v:Stat) =

    float__bin bb v.mean
    float__bin bb v.middle
    float__bin bb v.var
    float__bin bb v.median
    float__bin bb v.qinf
    float__bin bb v.qsup
    float__bin bb v.oinf
    float__bin bb v.osup
    float__bin bb v.inf
    float__bin bb v.sup
    int32__bin bb v.count
    Histogram__bin bb v.histogram
    
    array__bin (int32__bin) bb v.histogramData
    ()

let bin__Stat (bi:BinIndexed):Stat =
    let bin,index = bi

    {
        mean = 
            bi
            |> bin__float
        middle = 
            bi
            |> bin__float
        var = 
            bi
            |> bin__float
        median = 
            bi
            |> bin__float
        qinf = 
            bi
            |> bin__float
        qsup = 
            bi
            |> bin__float
        oinf = 
            bi
            |> bin__float
        osup = 
            bi
            |> bin__float
        inf = 
            bi
            |> bin__float
        sup = 
            bi
            |> bin__float
        count = 
            bi
            |> bin__int32
        histogram = 
            bi
            |> bin__Histogram
        histogramData = 
            bi
            |> bin__array (bin__int32)
    }

let Stat__json (v:Stat) =

    [|  ("mean",float__json v.mean)
        ("middle",float__json v.middle)
        ("var",float__json v.var)
        ("median",float__json v.median)
        ("qinf",float__json v.qinf)
        ("qsup",float__json v.qsup)
        ("oinf",float__json v.oinf)
        ("osup",float__json v.osup)
        ("inf",float__json v.inf)
        ("sup",float__json v.sup)
        ("count",int32__json v.count)
        ("histogram",Histogram__json v.histogram)
        ("histogramData",array__json (int32__json) v.histogramData)
         |]
    |> Json.Braket

let Stat__jsonTbw (w:TextBlockWriter) (v:Stat) =
    json__str w (Stat__json v)

let Stat__jsonStr (v:Stat) =
    (Stat__json v) |> json__strFinal


let json__Stato (json:Json):Stat option =
    let fields = json |> json__items

    let mutable passOptions = true

    let meano =
        match json__tryFindByName json "mean" with
        | None ->
            passOptions <- false
            None
        | Some v -> 
            match v |> json__floato with
            | Some res -> Some res
            | None ->
                passOptions <- false
                None

    let middleo =
        match json__tryFindByName json "middle" with
        | None ->
            passOptions <- false
            None
        | Some v -> 
            match v |> json__floato with
            | Some res -> Some res
            | None ->
                passOptions <- false
                None

    let varo =
        match json__tryFindByName json "var" with
        | None ->
            passOptions <- false
            None
        | Some v -> 
            match v |> json__floato with
            | Some res -> Some res
            | None ->
                passOptions <- false
                None

    let mediano =
        match json__tryFindByName json "median" with
        | None ->
            passOptions <- false
            None
        | Some v -> 
            match v |> json__floato with
            | Some res -> Some res
            | None ->
                passOptions <- false
                None

    let qinfo =
        match json__tryFindByName json "qinf" with
        | None ->
            passOptions <- false
            None
        | Some v -> 
            match v |> json__floato with
            | Some res -> Some res
            | None ->
                passOptions <- false
                None

    let qsupo =
        match json__tryFindByName json "qsup" with
        | None ->
            passOptions <- false
            None
        | Some v -> 
            match v |> json__floato with
            | Some res -> Some res
            | None ->
                passOptions <- false
                None

    let oinfo =
        match json__tryFindByName json "oinf" with
        | None ->
            passOptions <- false
            None
        | Some v -> 
            match v |> json__floato with
            | Some res -> Some res
            | None ->
                passOptions <- false
                None

    let osupo =
        match json__tryFindByName json "osup" with
        | None ->
            passOptions <- false
            None
        | Some v -> 
            match v |> json__floato with
            | Some res -> Some res
            | None ->
                passOptions <- false
                None

    let info =
        match json__tryFindByName json "inf" with
        | None ->
            passOptions <- false
            None
        | Some v -> 
            match v |> json__floato with
            | Some res -> Some res
            | None ->
                passOptions <- false
                None

    let supo =
        match json__tryFindByName json "sup" with
        | None ->
            passOptions <- false
            None
        | Some v -> 
            match v |> json__floato with
            | Some res -> Some res
            | None ->
                passOptions <- false
                None

    let counto =
        match json__tryFindByName json "count" with
        | None ->
            passOptions <- false
            None
        | Some v -> 
            match v |> json__int32o with
            | Some res -> Some res
            | None ->
                passOptions <- false
                None

    let histogramo =
        match json__tryFindByName json "histogram" with
        | None ->
            passOptions <- false
            None
        | Some v -> 
            match v |> json__Histogramo with
            | Some res -> Some res
            | None ->
                passOptions <- false
                None

    let histogramDatao =
        match json__tryFindByName json "histogramData" with
        | None ->
            passOptions <- false
            None
        | Some v -> 
            match v |> json__arrayo (json__int32o) with
            | Some res -> Some res
            | None ->
                passOptions <- false
                None

    if passOptions then
        ({
            mean = meano.Value
            middle = middleo.Value
            var = varo.Value
            median = mediano.Value
            qinf = qinfo.Value
            qsup = qsupo.Value
            oinf = oinfo.Value
            osup = osupo.Value
            inf = info.Value
            sup = supo.Value
            count = counto.Value
            histogram = histogramo.Value
            histogramData = histogramDatao.Value }:Stat) |> Some
    else
        None

let Stat_clone src =
    let bb = new BytesBuilder()
    Stat__bin bb src
    bin__Stat (bb.bytes(),ref 0)

// [SpotInStat] Structure

let SpotInStat_empty(): SpotInStat =
    {
        deviation = 0.0
        spot = 0.0
        anchor = 0.0
        digit = 0
        unit = ""
        stat = Stat_empty()
    }

let SpotInStat__bin (bb:BytesBuilder) (v:SpotInStat) =

    float__bin bb v.deviation
    float__bin bb v.spot
    float__bin bb v.anchor
    int32__bin bb v.digit
    str__bin bb v.unit
    Stat__bin bb v.stat
    ()

let bin__SpotInStat (bi:BinIndexed):SpotInStat =
    let bin,index = bi

    {
        deviation = 
            bi
            |> bin__float
        spot = 
            bi
            |> bin__float
        anchor = 
            bi
            |> bin__float
        digit = 
            bi
            |> bin__int32
        unit = 
            bi
            |> bin__str
        stat = 
            bi
            |> bin__Stat
    }

let SpotInStat__json (v:SpotInStat) =

    [|  ("deviation",float__json v.deviation)
        ("spot",float__json v.spot)
        ("anchor",float__json v.anchor)
        ("digit",int32__json v.digit)
        ("unit",str__json v.unit)
        ("stat",Stat__json v.stat)
         |]
    |> Json.Braket

let SpotInStat__jsonTbw (w:TextBlockWriter) (v:SpotInStat) =
    json__str w (SpotInStat__json v)

let SpotInStat__jsonStr (v:SpotInStat) =
    (SpotInStat__json v) |> json__strFinal


let json__SpotInStato (json:Json):SpotInStat option =
    let fields = json |> json__items

    let mutable passOptions = true

    let deviationo =
        match json__tryFindByName json "deviation" with
        | None ->
            passOptions <- false
            None
        | Some v -> 
            match v |> json__floato with
            | Some res -> Some res
            | None ->
                passOptions <- false
                None

    let spoto =
        match json__tryFindByName json "spot" with
        | None ->
            passOptions <- false
            None
        | Some v -> 
            match v |> json__floato with
            | Some res -> Some res
            | None ->
                passOptions <- false
                None

    let anchoro =
        match json__tryFindByName json "anchor" with
        | None ->
            passOptions <- false
            None
        | Some v -> 
            match v |> json__floato with
            | Some res -> Some res
            | None ->
                passOptions <- false
                None

    let digito =
        match json__tryFindByName json "digit" with
        | None ->
            passOptions <- false
            None
        | Some v -> 
            match v |> json__int32o with
            | Some res -> Some res
            | None ->
                passOptions <- false
                None

    let unito =
        match json__tryFindByName json "unit" with
        | None ->
            passOptions <- false
            None
        | Some v -> 
            match v |> json__stro with
            | Some res -> Some res
            | None ->
                passOptions <- false
                None

    let stato =
        match json__tryFindByName json "stat" with
        | None ->
            passOptions <- false
            None
        | Some v -> 
            match v |> json__Stato with
            | Some res -> Some res
            | None ->
                passOptions <- false
                None

    if passOptions then
        ({
            deviation = deviationo.Value
            spot = spoto.Value
            anchor = anchoro.Value
            digit = digito.Value
            unit = unito.Value
            stat = stato.Value }:SpotInStat) |> Some
    else
        None

let SpotInStat_clone src =
    let bb = new BytesBuilder()
    SpotInStat__bin bb src
    bin__SpotInStat (bb.bytes(),ref 0)


