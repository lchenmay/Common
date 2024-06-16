module Util.IA

open System
open System.Collections.Generic
open System.Collections.Concurrent

open Util.Cat
open Util.Bin
open Util.Zmq

//Console.OutputEncoding <- System.Text.Encoding.Unicode
//let mutable output (s:string) = Console.WriteLine s
let mutable output =
    (fun (s:string) -> System.Diagnostics.Debug.WriteLine s)

// SD = Serialization / Deserialization
type ModeSD = 
| Json
| Bin

type Packet<'Fact,'Msg> = {
msg: 'Msg
facts: 'Fact[]
sendTimestamp: DateTime
recvTimestamp: DateTime
mode: ModeSD }

type IncomingFacts<'T> = 'T[] -> unit

type IaClient<'T> = {
wsClient: WsClient
Fact__bin: BytesBuilder -> 'T -> unit
bin__Fact: BinIndexed -> 'T
incomingFacts: List<IncomingFacts<'T>> }

let incomingFacts client binOrTxt (bin:byte[]) = 

    //"<< Incoming Server Broadcast << " + bin.Length.ToString() + " bytes"
    //|> output
    if binOrTxt then
        let items = 
            (bin,ref 0)
            |> bin__array client.bin__Fact

        client.incomingFacts.ToArray()
        |> Array.iter(fun h -> h items)

let clientPushFact__server client fact = 

    let bin = 
        let bb = new BytesBuilder()
        [| fact |]
        |> array__bin client.Fact__bin bb
        bb.bytes()

    //">> Server >> pushing " + bin.Length.ToString() + " bytes"
    //|> output

    WsPush client.wsClient (WsMsgType.Binary, bin)

let connect network = 
    launchWsClient output network.wsClient (incomingFacts network)


