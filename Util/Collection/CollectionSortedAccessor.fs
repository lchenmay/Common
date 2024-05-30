module Util.CollectionSortedAccessor 

open System
open System.Collections.Generic

open Util.Concurrent
open Util.CollectionModDict



type SortedAccessor<'k,'v>(exp2) =

    let moddict64 = createMDInt64<'v>(exp2)
    let bykey = new SortedDictionary<'k,int64>()

    member this.tryget(id) = moddict64.tryget(id)

    member this.Item
        with get(key) =
            let p,partition = moddict64.modulo(key)
            partition.[key].data

    member this.containsid(id) = moddict64.ContainsKey(id)
        
    member this.containskey(key) = bykey.ContainsKey(key)

    member this.array() = moddict64.array()

    member this.ids() = moddict64.keys()
    member this.keys() = bykey.Keys
    member this.count() = moddict64.count

    member this.iter(f) = moddict64.iter(f)
    member this.tryfind(f) = moddict64.tryfind(f)
    member this.find(f) = moddict64.filter(f)

    member this.append(id,key)(value) =
        moddict64.[id] <- value
        lock(bykey)(fun()->
            if(bykey.ContainsKey(key) = false)then
                bykey.[key] <- id)

    member this.trygetkey(key) =
        if(bykey.ContainsKey(key))then
            let id = bykey.[key]
            Some(id,moddict64.[id])
        else
            None

    member this.loadkey(key) =
        let id = bykey.[key]
        id,moddict64.[id]

    member this.setkey(id,key,value) =
        lock(bykey)(fun()->
            if(bykey.ContainsKey(key)=false)then
                bykey.Add(key,id)
            moddict64.[id]<-value)

    member this.remove(id,key) = 
        lock(bykey)(fun()->
            if(bykey.ContainsKey(key))then
                bykey.Remove(key) |> ignore
            moddict64.remove(id))

    member this.monitor(max,populater)(key) = 
        let data = 
            let sb = new System.Text.StringBuilder()
            //(this.take(true)(max)).ToArray()
            //|> Array.map(fun item -> "[" + populater(item) + "]")
            //|> String.Concat
            //|> sb.Append
            //|> ignore
            //sb.Append(" ... ") |> ignore
            //(this.take(false)(5)).ToArray()
            //|> Array.rev
            //|> Array.map(fun item -> "[" + populater(item) + "]")
            //|> String.Concat
            //|> sb.Append
            //|> ignore
            sb.ToString()

        [|  "{\"Key\":\"" + key + "\"";
            //",\"Size\":" + this.size.ToString();
            //",\"Total\":" + this.total.ToString();
            //",\"Outdate\":" + this.outdate.ToString();
            ",\"Data\":\"" + data + "\"}"  |]
        |> String.Concat


let create_sortedaccessor
    (loading:unit-> (int64*'k*'v)[])(exp2) =
    let res = new SortedAccessor<'k,'v>(exp2)
    loading()
    |> Array.iter(fun item ->
        let id,k,v = item
        res.append(id,k)(v))
    res
