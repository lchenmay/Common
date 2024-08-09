module Util.Test

open Util.Perf
open Util.Json

let sampleScript = """
{
    "url": "http://localhost:1234",
    "threads":[
        "thread":{
            "caption":"View",
            "interval":3000,
            "parallel": 100,
            "seq":[
                "step":{
                }
            ]
        },
        "thread":{
        }]}
"""

let script__task script = 

    let url = tryFindStrByAtt "url" script
    let threads = 
        tryFindAryByAtt "threads" script
        |> Array.filter(fun i -> 


            true)

    if url.Length * threads.Length > 0 then


        ()


    ()