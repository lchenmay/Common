module Util.ADT // Algebraic Data Type

let oPipeline hSome hNone o = 
    match o with
    | Some v -> v |> hSome
    | None -> () |> hNone

let oPipelineSome h o = 
    match o with
    | Some v -> v |> h |> Some
    | None -> None

let oPipelineNone h o = 
    match o with
    | Some v -> v |> Some
    | None -> () |> h

let handleroDefault defaultRes ho = 
    match ho with
    | Some h -> h()
    | None -> defaultRes

let handlero ho param = 
    match ho with
    | Some h -> h param
    | None -> ()

let oPipelineNoneHandlero defaultRes ho o = 
    o
    |> oPipelineNone (fun _ ->
        match ho with
        | Some h -> h()
        | None -> defaultRes
        |> Some)
