module Util.Collection 

open System
open System.Collections.Concurrent
open System.Collections.Generic

open System.Collections.Immutable
open System.Linq

let filter<'A,'B> matcher (data:'A[]) = 
    let res = List<'B>()
    data
    |> Array.iter(fun t ->
        match matcher t with
        | Some t -> res.Add t
        | None -> ())
    res.ToArray()

let arrayKVP__dict array =
    let dict = new Dictionary<'K,'T>()
    array 
    |> Array.iter(fun i -> dict.Add(fst i,snd i))
    dict

let array__dict (key:'T -> 'K) list =
    let dict = new Dictionary<'K,'T>()
    list |> Array.iter(fun i -> dict.Add(key i, i))
    dict

let array__sorteddict(key:'T->'K) list =
    let dict = new SortedDictionary<'K,'T>()
    list |> Array.iter(fun i -> dict.Add(key(i), i))
    dict

let array__indexed_dict(list:'T[]) =
    let dict = new SortedDictionary<int,'T>()
    [| 0..list.Length - 1 |] |> Array.iter(fun i -> dict.Add(i, list.[i]))
    dict

let array_contains list item =
    let map = list |> Array.map(fun t -> t, true) |> Map.ofSeq
    map.ContainsKey item
        
let arrayIntersect (arr1:'T[]) (arr2:'T[]) =
    arr2
    |> Array.except arr1 
    |> Array.except
    <| arr2

let seqIntersect (arr1:'T seq) (arr2:'T seq) =
    arr2
    |> Seq.except arr1 
    |> Seq.except
    <| arr2

let arrayIntersectMany (aa: 'T[][]) =
    Array.reduce arrayIntersect aa
        
let seqIntersectMany (aa: 'T seq seq) =
    Seq.reduce seqIntersect aa
    
let ImmutableSortedSetIntersect (iss1:ImmutableSortedSet<'T>) (iss2:ImmutableSortedSet<'T>) =
    iss2.Intersect iss1

let ImmutableSortedSetIntersectMany (aa: ImmutableSortedSet<'T> seq) =
    Seq.reduce ImmutableSortedSetIntersect aa

let ImmutableSortedUnion (iss1:ImmutableSortedSet<'T>) (iss2:ImmutableSortedSet<'T>) =
    iss2.Union iss1

let ImmutableSortedSetUnionMany (isss: ImmutableSortedSet<'T> seq) =
    let builder = ImmutableSortedSet.CreateBuilder<'T>()
    
    for set in isss do
        builder.UnionWith(set)
        
    builder.ToImmutable()

/// whether B contains any elements of A
let arraysSomeContains (A:'T[]) (B:'T[]) =
    let intersectCount =
        Array.except A B
        |> Array.length

    intersectCount < B.Length

let duodict__dict(duodict:Dictionary<'K1, Dictionary<'K2, 'V>>) =
    let dict = new Dictionary<'K2, 'V>()
    duodict.ToArray() |> Array.iter(fun item ->
        item.Value.ToArray() |> Array.iter(fun kvp -> dict.Add(kvp.Key, kvp.Value)))
    dict

let sortarray array =
    let sd = new SortedDictionary<'T, bool>()
    array |> Array.iter(fun t -> sd.Add(t, true))
    sd.Keys.ToArray()

let sortarray_withkey(array:'V[], key) =
    let sd = new SortedDictionary<'K, 'V>()
    array |> Array.iter(fun item -> sd.Add(key item, item))
    let ls = new List<'V>(sd.Values)
    ls.ToArray()

let array__sorted(list:(DateTime*'T)[]) = 

    let sd = new SortedDictionary<DateTime,List<'T>>()
    
    list
    |> Array.iter(fun item ->
        let k = fst item
        let v = snd item
        if(sd.ContainsKey(k)=false)then
            sd.Add(k,new List<'T>())
        sd.[k].Add(v))

    let res = new List<DateTime*'T>()
    
    sd.Keys.ToArray()
    |> Array.iter(fun k ->
        sd.[k].ToArray()
        |> Array.iter(fun v -> res.Add(k,v)))

    res.ToArray()

// 数组集合随机获取元素  Begin
let swap(array:'T[],index1:int,index2:int)=
    let temp:'T = array.[index2]
    array.[index2] <- array.[index1]
    array.[index1] <- temp

let array__sortrandom(array:'T[])=
    let mutable randomIndex=0
    for i = array.Length to 1 do
        let r = new Random();
        randomIndex <- r.Next(0,i)
        swap(array,randomIndex,i)
    array

let array__get_random_childs (array:'T[],count)=
    let temparray:'T[]= Array.zeroCreate array.Length
    let temp_resultarray:'T[]= Array.zeroCreate count

    array.CopyTo(temparray,0)
    array__sortrandom array |>ignore
    System.Array.Copy(temparray,temp_resultarray,count)
    temp_resultarray


let indexof comparer (length,localizer,m:'V[],starting) =
    let res =
        [| starting..length - m.Length |]
        |> Array.tryFindIndex(fun i->
            let mutable found = true
            [| 0..m.Length - 1 |]
            |> Array.iter(fun j->
                if comparer(localizer(i+j),m.[j]) = false then
                    found<-false)
            found)
    if res.IsSome then
        Some(starting + res.Value)
    else
        None

let array_indexof comparer (ra:'V[],m:'V[],starting) =
    indexof comparer (ra.Length,(fun i -> ra.[i]),m,starting)

let array_byte_indexof(ra:byte[],m:byte[],starting) =
    indexof(fun(a,b) -> a = b)(ra.Length,(fun i-> ra.[i]),m,starting)

let dictTryLoad (dict:ConcurrentDictionary<'a,'b>) key = 
    if dict.ContainsKey key then
        Some dict.[key]
    else
        None

let dict__clone (dict:Dictionary<'K,'V>) = 
    let res = Dictionary<'K,'V>()

    lock dict (fun _ -> 
        dict 
        |> Seq.toArray)
    |> Array.iter(fun kvp -> res.Add(kvp.Key,kvp.Value))

    res

let tryCheckOrCreateDictionary 
    creator (dict:Dictionary<'K,'V>) 
    key = 
    if dict.ContainsKey key = false then
        match creator key with
        | Some v -> 
            dict[key] <- v
            Some v
        | None -> None
    else
        Some dict[key]

let tryCheckOrCreateSortedDictionary 
    creator (dict:SortedDictionary<'K,'V>) 
    key = 
    if dict.ContainsKey key = false then
        match creator key with
        | Some v -> 
            dict[key] <- v
            Some v
        | None -> None
    else
        Some dict[key]

let checkOrCreateDictionary 
    creator (dict:Dictionary<'K,'V>) 
    key = 
    if dict.ContainsKey key = false then
        dict[key] <- creator key
    dict[key]

let checkOrCreateSortedDictionary 
    creator (dict:SortedDictionary<'K,'V>) 
    key = 
    if dict.ContainsKey key = false then
        dict[key] <- creator key
    dict[key]


//<string> ================================================

let checkfield(fields:Dictionary<string,string>) key =
    if fields.ContainsKey key = false then
        ""
    else
        fields.[key]


type StreamBuilder<'T>() =

    let mutable count = 0
    let mutable buffer = new List<'T[]>()

    member this.append bs =
        buffer.Add bs
        count <- count + bs.Length

    member this.length() = count

    member this.bytes() = buffer.ToArray() |> Array.concat

    member this.fetch(index,count) =
        let array = this.bytes()
        [| 0..count - 1 |] |> Array.map(fun i -> array.[i+index])

    member this.clear() =
        buffer.Clear()
        count <- 0

    member this.dump() =
        let bs = buffer.ToArray() |> Array.concat
        buffer.Clear()
        count <- 0
        bs

let dequeueAll (queue:ConcurrentQueue<'T>) = 
    let items = new List<'T>()
    let mutable yes = true
    while yes do
        let v = queue.TryDequeue()
        yes <- fst v
        if yes then
            v |> snd |> items.Add
    items.ToArray()
