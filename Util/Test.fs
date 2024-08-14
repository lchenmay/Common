module Util.Test

open Util.Perf
open Util.Text
open Util.Json
open Util.Concurrent
open Util.Http
open Util.HttpClient

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

type Step = unit

type Thread = {
interval: int
paral: int
sequence: Step[]
caption: string }

type Task = {
url: string
threads: Thread[] }

let script__task script = 
    {
        url = tryFindStrByAtt "url" script
        threads = 
            tryFindAryByAtt "threads" script
            |> Array.map(tryFindByAtt "thread")
            |> Array.filter(fun o -> o.IsSome)
            |> Array.map(fun o -> 
                let json = o.Value |> snd
            
                {   interval = tryFindStrByAtt "caption" json |> parse_int32
                    paral = tryFindStrByAtt "parallel" json |> parse_int32
                    sequence = [||]
                    caption = tryFindStrByAtt "caption" json }) }

let run task =
    let hc = empty__HttpClient()

    task.threads
    |> Array.iter(fun thread -> 

        
    
        ())


