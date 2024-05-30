module Util.ObjectConvert

open System
open System.Runtime.CompilerServices

let GetBool(data)=
    match data.ToString().Trim().ToLower() with
    | "0"-> false
    | "1"-> true
    | "是"-> true
    | "否"-> false
    | "yes"-> true
    | "no"-> false
    | "true"-> true
    | "false"-> false
    | ""-> false
    | _-> false
[<Extension>]
type Object()=
    [<Extension>]
    static member ToFloat(data) =
        if data=null then
            0.0
        else
            let mutable result=0.0
            Double.TryParse(data.ToString(),&result) |>ignore
            result
        
    [<Extension>]
    static member ToInt64(data) =
        if data=null then
            0L
        else
            let mutable result=0L
            Int64.TryParse(data.ToString(),&result) |> ignore
            result

    [<Extension>]
    static member ToInt32(data) =
        if data=null then
            0
        else
            try 
                Convert.ToInt32(data)
            with
            | ex-> 0

    [<Extension>]
    static member ToDecimal(data) =
        if data=null then
            0M
        else
            let mutable result= 0M
            Decimal.TryParse(data,&result) |> ignore
            result

    [<Extension>]
    static member ToDate(data) =
        if data=null then
            DateTime.MinValue
        else
            let mutable result= DateTime.MinValue
            DateTime.TryParse(data.ToString(),&result) |> ignore
            result


        
    [<Extension>]
    static member ToBool(data) =
        if data=null then
            false
        else
            GetBool(data)
                        
                
