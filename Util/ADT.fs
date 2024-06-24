module Util.ADT // Algebraic Data Type

let optionProcess hSome hNone o = 
    match o with
    | Some v -> v |> hSome
    | None -> hNone()

let optionProcessSome h o = 
    match o with
    | Some v -> v |> h |> Some
    | None -> None

