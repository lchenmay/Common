﻿module Util.Bin

open System
open System.IO
open System.Collections
open System.Collections.Generic
open System.Text
open System.Numerics

open Util.Perf
open Util.Collection
open Util.Json

type ByteArrayComparer() = 
    interface IEqualityComparer<byte[]> with
        member this.Equals(alpha:byte[],beta:byte[]) = 
            StructuralComparisons.StructuralEqualityComparer.Equals(alpha,beta)

        member this.GetHashCode(bin:byte[]) = 
            StructuralComparisons.StructuralEqualityComparer.GetHashCode bin

let binComparer = new ByteArrayComparer()

type IDByteArrayComparer() = 
    interface IEqualityComparer<int64*byte[]> with
        member this.Equals(alpha:(int64*byte[]),beta:(int64*byte[])) = 
            fst(alpha) = fst(beta) &&
            StructuralComparisons.StructuralEqualityComparer.Equals(snd(alpha),snd(beta))

        member this.GetHashCode(k:(int64*byte[])) = 
            [fst(k) |> BitConverter.GetBytes; snd(k)]
            |> Array.concat
            |> StructuralComparisons.StructuralEqualityComparer.GetHashCode

let idbinComparer = new IDByteArrayComparer()

type BinIndexed = byte[] * Ref<int>

[<Literal>]
let CR = 0x0Duy

[<Literal>]
let LF = 0x0Auy

let CRLF = [| CR; LF |]
let BLANKLINE = [| CR; LF; CR; LF |]

let bin__span (bin:byte[]) = (bin |> System.ReadOnlyMemory).Span

let padLeft length (src:byte[]) = 
    if length <= src.Length then
        src
    else
        [|  Array.zeroCreate (length - src.Length)
            src |]
        |> Array.concat

let padRight length (src:byte[]) = 
    if length <= src.Length then
        src
    else
        [|  src
            Array.zeroCreate (length - src.Length) |]
        |> Array.concat

type BytesBuilder() =

    let mutable count = 0
    let mutable buffer = new List<byte[]>()

    member this.appendone b =
        buffer.Add [|b|]
        count <- count + 1

    member this.append bs =
        buffer.Add bs
        count <- count + bs.Length

    member this.append(bs:byte[],count) =
        if bs.Length = count then
            buffer.Add bs
        else
            buffer.Add(bs |> Array.take count)

    member this.insert bin = 
        buffer.Insert(0,bin)
        count <- count + bin.Length

    member this.length() = count

    member this.bytes() = buffer.ToArray() |> Array.concat

    member this.fetch(index,count) =
        let array = this.bytes()
        [| 0..count-1 |] |> Array.map(fun i -> array.[i+index])

    member this.clear() =
        buffer.Clear()
        count <- 0

    member this.dump() =
        let bs = buffer.ToArray() |> Array.concat
        buffer.Clear()
        count <- 0
        bs

let buildBin builder = 
    let bb = new BytesBuilder()
    builder bb
    bb.bytes()

let buildBinFun builder = 
    (fun () ->
        let bb = new BytesBuilder()
        builder bb
        bb.bytes())

let bytes__hex(bytes:byte[]) =
    bytes |> Array.fold (fun state x-> state + sprintf "%02X" x) ""

let hex__bytes (hex:string) = 
    hex.ToCharArray() 
    |> Array.windowed 2
    |> Array.mapi (fun i j -> (i,j))
    |> Array.filter (fun (i,j) -> i % 2=0)
    |> Array.map (fun (_,j) -> Byte.Parse(new System.String(j),System.Globalization.NumberStyles.AllowHexSpecifier))
    |> Array.ofSeq

// ==== [Hex] ========================================

let private hex_line(sb:List<string>,buffer:char[],col) =
    let mutable s = ""
    [| 0..buffer.Length-1 |] 
    |> Array.iter(fun k->
        s <- s+ buffer.[k].ToString())
    sb.Add(s+Util.Text.crlf)
    [| 0..buffer.Length - 1 |] 
    |> Array.map(fun i-> ' ')

let hex(bs:byte[]) =
    let sb = new List<string>()

    sb.Add(Util.Text.crlf)

    let col = 16
    let hex_length = 4
    let dec_length = 4
    let row_length = hex_length + 1 + dec_length

    let mutable buffer = 
        [| 0..row_length + 1 + col * 4 |] 
        |> Array.map(fun i-> ' ')
    let mutable row = 0

    [| 0 .. bs.Length - 1 |]
    |> Array.iter(fun ii ->

        let i = ii % col

        let number = row*col
        number.ToString("X").PadLeft(hex_length,'0').ToCharArray().CopyTo(buffer,0)
        number.ToString("0").PadLeft(dec_length,'0').ToCharArray().CopyTo(buffer,hex_length + 1)

        let b = bs.[ii]
        let cs =
            match b with
            | CR -> [| '/'; 'r' |]
            | LF -> [| '/'; 'n' |]
            | 32uy -> "  ".ToCharArray()
            | _ -> (b.ToString "X").PadLeft(2,'0').ToCharArray()
        cs.CopyTo(buffer, row_length + 1 + i * 3)

        let c =
            let bb = b |> int
            if (bb = 0)then
                '.'
            else if (bb < 32 || bb > 126)then
                '?'
            else
                b |> char

        buffer.[row_length + 1 + i + col * 3 + 1] <- c

        if((i+1)%col=0)then
            row <- row + 1
            buffer <- hex_line(sb,buffer,col))

    buffer <- hex_line(sb,buffer,col)
    sb.Add(Util.Text.crlf + bs.Length.ToString("N0") + " bytes" + Util.Text.crlf)
    sb |> Seq.filter(fun item->item<>null)|> String.Concat


let readstream_until_pattern
    (stream:Stream,buffer:byte[])
    (bin:List<byte>,pattern:byte[],starting) =
    match array_byte_indexof(bin.ToArray(),pattern,starting) with
    | Some(v) -> Some(v)
    | _ ->
        let mutable res = None
        let mutable keep = true
        while(keep)do
            let count = stream.Read(buffer,0,buffer.Length)
            if(count = 0)then
                keep <- false
            else
                bin.AddRange(buffer |> Array.take(count))
                res <- array_byte_indexof(bin.ToArray(),pattern,starting)
                if(res.IsSome)then
                    keep <- false
        res

let str__hash(s:string) = 
    let bs = s |> System.Text.Encoding.UTF8.GetBytes
    use md5 = System.Security.Cryptography.MD5.Create()
    (StringBuilder(), md5.ComputeHash(bs))
    ||> Array.fold (fun sb b -> sb.Append(b.ToString("x2")))
    |> string

(*
██████████████████████████████████████████████████████████████████████████████
██████████████████████████████████████████████████████████████████████████████
████       ███████████████████████████████████████████████████████████████████
██████████████████████████████████████████████████████████████████████████████
██████████████████████████████████████████████████████████████████████████████
*)

let int32__bin (bb:BytesBuilder) (f:int) = f |> BitConverter.GetBytes |> bb.append
let bin__int32 (bi:BinIndexed) =
    let bin,index = bi
    let res = BitConverter.ToInt32(bin,index.Value)
    index.Value <- index.Value + 4
    res

let uint32__bin (bb:BytesBuilder) (f:uint) = f |> BitConverter.GetBytes |> bb.append
let bin__uint32 (bi:BinIndexed) =
    let bin,index = bi
    let res = BitConverter.ToUInt32(bin,index.Value)
    index.Value <- index.Value + 4
    res

let char__bin (bb:BytesBuilder) (c:char) = c |> BitConverter.GetBytes|> bb.append
let bin__char (bi:BinIndexed) =
    let bin,index = bi
    let res = BitConverter.ToChar(bin,index.Value)
    index.Value <- index.Value + 2
    res

let int64__bin (bb:BytesBuilder) (f:int64) = f |> BitConverter.GetBytes |> bb.append
let bin__int64(bi:BinIndexed) =
    let bin,index = bi
    let res = BitConverter.ToInt64(bin,index.Value)
    index.Value <- index.Value + 8
    res

let float__bin (bb:BytesBuilder) (f:float) = f |> BitConverter.GetBytes |> bb.append
let bin__float(bi:BinIndexed) =
    let bin,index = bi
    let res = BitConverter.ToDouble(bin,index.Value)
    index.Value <- index.Value + 8
    res

let float32__bin (bb:BytesBuilder) (f:float32) = f |> BitConverter.GetBytes |> bb.append
let bin__float32(bi:BinIndexed) =
    let bin,index = bi
    let res = BitConverter.ToSingle(bin,index.Value)
    index.Value <- index.Value + 4
    res

let bigint__bin (bb:BytesBuilder) (f:BigInteger) = 
    let bs = f.ToByteArray()
    bs.Length |> BitConverter.GetBytes |> bb.append
    bs |> bb.append
let bin__bigint(bi:BinIndexed) =
    let bin,index = bi
    let length = BitConverter.ToInt32(bin,index.Value)
    index.Value <- index.Value + 4
    let res = new BigInteger(Array.sub bin index.Value length)
    index.Value <- index.Value + length
    res

let bool__bin(bb:BytesBuilder) (f:bool) = f |> BitConverter.GetBytes |> bb.append
let bin__bool(bi:BinIndexed) =
    let bin,index = bi
    let res = BitConverter.ToBoolean(bin,index.Value)
    index.Value <- index.Value + 1
    res

let str__bin (bb:BytesBuilder) (s:string) =
    let bs = s |> Encoding.UTF8.GetBytes
    bs.Length |> BitConverter.GetBytes |> bb.append
    bs |> bb.append
let bin__str(bi:BinIndexed) =
    let bin,index = bi
    let count = BitConverter.ToInt32(bin,index.Value)
    index.Value <- index.Value + 4
    let res = Encoding.UTF8.GetString(bin,index.Value,count)
    index.Value <- index.Value + count
    res

let json__bin (bb:BytesBuilder) (json:Json) =
    json
    |> json__strFinal
    |> str__bin bb
let bin__json(bi:BinIndexed) =
    bi
    |> bin__str
    |> str__root


let bytes__bin (bb:BytesBuilder) (bs:byte[]) =
    bs.Length |> BitConverter.GetBytes |> bb.append
    bs |> bb.append
let bin__bytes(bi:BinIndexed) =
    let bin,index = bi
    let count = BitConverter.ToInt32(bin,index.Value)
    index.Value <- index.Value + 4
    let res = Array.sub bin (index.Value) count
    index.Value <- index.Value + count
    res


let DateTime__bin (bb:BytesBuilder) (f:DateTime) = f.Ticks |> BitConverter.GetBytes |> bb.append
let bin__DateTime(bi:BinIndexed) =
    let bin,index = bi
    let res = BitConverter.ToInt64(bin,index.Value)
    index.Value <- index.Value + 8
    res |> DateTime.FromBinary

let bs__gzip bs = 

    let ms = new MemoryStream()
    let stream = new System.IO.Compression.GZipStream(ms, System.IO.Compression.CompressionMode.Compress, true)
    stream.Write(bs, 0, bs.Length)
    stream.Close()
    ms.ToArray()

(*
██████████████████████████████████████████████████████████████████████████████
██████████████████████████████████████████████████████████████████████████████
████       ███████████████████████████████████████████████████████████████████
██████████████████████████████████████████████████████████████████████████████
██████████████████████████████████████████████████████████████████████████████
*)

let array__bin item__bin (bb:BytesBuilder) (array:'T[]) =
    int32__bin bb array.Length
    array
    |> Array.iter(item__bin bb)

let bin__array bin__item (bi:BinIndexed) = 
    let bin,_ = bi
    if bin.Length < 4 then
        [||]
    else
        [| 0..bin__int32(bi) - 1 |]
        |> Array.map(fun i ->
            bin__item bi)

let Option__bin<'T> item__bin (bb:BytesBuilder) (o:'T option) =
    match o with
    | Some v -> 
        bool__bin bb true
        item__bin bb v
    | None -> 
        bool__bin bb false

let bin__Option<'T> bin__item (bi:BinIndexed):'T option = 
    if bin__bool bi then
        bin__item bi
        |> Some
    else
        None

let List__bin<'T> item__bin (bb:BytesBuilder) (list:List<'T>) =
    list.ToArray() |> array__bin item__bin bb 

let bin__List<'T> bin__item (bi:BinIndexed) = 
    new List<'T>(bin__array bin__item bi)

let bin__ConcurrentBag<'T> (bin__data:BinIndexed -> 'T) bi = 

    let bag = new System.Collections.Concurrent.ConcurrentBag<'T>()
        
    bin__data |> bin__array <| bi
    |> Array.iter bag.Add

    bag

let concurrentBag__bin
    (bb:BytesBuilder)
    (bag:System.Collections.Concurrent.ConcurrentBag<'t>,item__bin) =
        bag.ToArray()
        |> array__bin item__bin bb
        
let Dictionary__bin
    key__bin 
    val__bin
    (bb:BytesBuilder)
    (dict:Dictionary<'k,'v>) =
        lock dict (fun _ ->
            int32__bin bb dict.Count
            dict.Keys
            |> Seq.toArray
            |> Array.iter(fun k -> 
                key__bin bb k
                val__bin bb (dict[k])))

let bin__Dictionary<'K,'V> 
    bin__key
    bin__val
    (dict:Dictionary<'K,'V>)
    bi = 
    lock dict (fun _ ->
        dict.Clear()
        [| 0 .. (bin__int32 bi) - 1 |]
        |> Array.iter(fun i -> 
            let k = bin__key bi
            let v = bin__val bi
            dict.Add(k,v)))

let bin__ConcurrentDictionary<'K,'V> 
    (bin__kvp:BinIndexed -> KeyValuePair<'K,'V>)
    (dict:System.Collections.Concurrent.ConcurrentDictionary<'K,'V>)
    bi = 
    lock dict (fun _ ->
        dict.Clear()
        bin__kvp |> bin__array <| bi
        |> Array.iter(fun kvp -> dict.[kvp.Key] <- kvp.Value))
        
let concurrentDictionary__bin
    (bb:BytesBuilder)
    (dict:System.Collections.Concurrent.ConcurrentDictionary<'k,'v>,kvp__bin) =
        dict.ToArray() 
        |> array__bin kvp__bin bb
