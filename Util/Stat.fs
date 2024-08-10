﻿module Util.Stat

open System.Collections.Generic

open Util.Bin
open Util.Json
open Util.Text

type Stat = {
mean: float
middle: float
var: float
median: float
ma: float
mb: float
mc: float
md: float
min: float
max: float
count: int }

type SpotInStat = {
deviation: float
spot: float
anchor: float
digit: int
unit: string
stat: Stat }


// [Stat] Structure

let Stat_empty(): Stat =
    {
        mean = 0.0
        middle = 0.0
        var = 0.0
        median = 0.0
        ma = 0.0
        mb = 0.0
        mc = 0.0
        md = 0.0
        min = 0.0
        max = 0.0
        count = 0
    }

let Stat__bin (bb:BytesBuilder) (v:Stat) =

    float__bin bb v.mean
    float__bin bb v.middle
    float__bin bb v.var
    float__bin bb v.median
    float__bin bb v.ma
    float__bin bb v.mb
    float__bin bb v.mc
    float__bin bb v.md
    float__bin bb v.min
    float__bin bb v.max
    int32__bin bb v.count

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
        ma =
            bi
            |> bin__float
        mb =
            bi
            |> bin__float
        mc =
            bi
            |> bin__float
        md =
            bi
            |> bin__float
        min =
            bi
            |> bin__float
        max =
            bi
            |> bin__float
        count =
            bi
            |> bin__int32
    }

let Stat__json (v:Stat) =

    [|  ("mean",float__json v.mean)
        ("middle",float__json v.middle)
        ("var",float__json v.var)
        ("median",float__json v.median)
        ("ma",float__json v.ma)
        ("mb",float__json v.mb)
        ("mc",float__json v.mc)
        ("md",float__json v.md)
        ("min",float__json v.min)
        ("max",float__json v.max)
        ("count",int32__json v.count)
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

    let mao =
        match json__tryFindByName json "ma" with
        | None ->
            passOptions <- false
            None
        | Some v -> 
            match v |> json__floato with
            | Some res -> Some res
            | None ->
                passOptions <- false
                None

    let mbo =
        match json__tryFindByName json "mb" with
        | None ->
            passOptions <- false
            None
        | Some v -> 
            match v |> json__floato with
            | Some res -> Some res
            | None ->
                passOptions <- false
                None

    let mco =
        match json__tryFindByName json "mc" with
        | None ->
            passOptions <- false
            None
        | Some v -> 
            match v |> json__floato with
            | Some res -> Some res
            | None ->
                passOptions <- false
                None

    let mdo =
        match json__tryFindByName json "md" with
        | None ->
            passOptions <- false
            None
        | Some v -> 
            match v |> json__floato with
            | Some res -> Some res
            | None ->
                passOptions <- false
                None

    let mino =
        match json__tryFindByName json "min" with
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

    if passOptions then
        {
            mean = meano.Value
            middle = middleo.Value
            var = varo.Value
            median = mediano.Value
            ma = mao.Value
            mb = mbo.Value
            mc = mco.Value
            md = mdo.Value
            min = mino.Value
            max = maxo.Value
            count = counto.Value} |> Some
    else
        None

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
        {
            deviation = deviationo.Value
            spot = spoto.Value
            anchor = anchoro.Value
            digit = digito.Value
            unit = unito.Value
            stat = stato.Value} |> Some
    else
        None


