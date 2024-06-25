module Util.ADT // Algebraic Data Type

let optionProcess hSome hNone o = 
    match o with
    | Some v -> v |> hSome
    | None -> hNone()

let optionProcessSome h o = 
    match o with
    | Some v -> v |> h |> Some
    | None -> None

let optionProcessNoneOption h o = 
    match o with
    | Some v -> v |> Some
    | None -> h()

let optionProcessNone h o = 
    match o with
    | Some v -> v
    | None -> h()


let optionProcessSomeHandler defaultRes o = 
    match o with
    | Some h -> h()
    | None -> defaultRes

let oPipelineNone h o = 
    match o with
    | Some v -> v |> Some
    | None -> h()

let oPipelineNoneHandlero defaultRes ho o = 
    o
    |> oPipelineNone (fun _ ->
        match ho with
        | Some h -> h()
        | None -> defaultRes
        |> Some)
