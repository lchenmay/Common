module Util.Cat

open System
open System.Collections.Generic


type Hom<'X,'Y> = 'X -> 'Y
type End<'X> = 'X -> 'X

type CtxWrapper<'ctx,'ex> =
| Suc of 'ctx
| Fail of 'ex * 'ctx

let (>>=) c f = 
    match c with
    | Suc x -> f x
    | Fail (ex,x) -> c

let (>=>) f g a = (f a) >>= g

let (<||>) f g a =
    match f a with
    | Suc x -> Ok x
    | Fail (ex,x) -> g a

let bind f = fun c -> c >>= f

let bindFail f m =
    match m with
    | Suc x -> m
    | Fail (ex,x) -> f x

type MaybeBuilder() =

    member this.Bind(x, f) =
        match x with
        | None -> None
        | Some a -> f a

    member this.Return x = Some x

let maybe = new MaybeBuilder()

type OrElseBuilder() =

    member this.ReturnFrom x = x

    member this.Combine(a, b) =
        match a with
        | Some _ -> a
        | None -> b

    member this.Delay f = f()

let or_else = new OrElseBuilder()

let ofOption error = 
    function Some s -> Ok s | None -> Error error

let combineAll results =
    results
    |> Seq.fold (fun state result -> 
        match state, result with
        | Ok acc, Ok res -> Ok <| res :: acc
        | Error e, _
        | _, Error e -> Error e
        ) (Ok [])

type ResultBuilder() =
    member __.Return x = Ok x

    member __.ReturnFrom(m: Result<_, _>) = m

    member __.Bind(m, f) = Result.bind f m
    member __.Bind((m, error): (Option<'T> * 'E), f) = m |> ofOption error |> Result.bind f

    member __.Zero() = None

    member __.Combine(m, f) = Result.bind f m

    member __.Delay(f: unit -> _) = f

    member __.Run f = f()

    member __.TryWith(m, h) =
        try __.ReturnFrom m
        with e -> h e

    member __.TryFinally(m, compensation) =
        try __.ReturnFrom m
        finally compensation()

    member __.Using(res: #IDisposable, body) =
        __.TryFinally(body res, fun () -> match res with null -> () | disp -> disp.Dispose())

    member __.While(guard, f) =
        if not (guard()) then Ok () else
        do f() |> ignore
        __.While(guard, f)

    member __.For(sequence:seq<_>, body) =
        __.Using(sequence.GetEnumerator(), fun enum -> __.While(enum.MoveNext, __.Delay(fun () -> body enum.Current)))

let result = new ResultBuilder()

let ifElse yes no predicate = 
    if predicate then
        yes()
    else
        no()

// ==============================================
// Option

let processOptionWithNone h none o =
    match o with
    | Some v -> h v
    | None -> none

let processOption h o =
    match o with
    | Some v -> h v
    | None -> ()

let pipeInto (exp,lambda) = exp |> lambda


