module Util.WebSocket

open LanguagePrimitives

open System
open System.IO
open System.Text
open System.Threading
open System.Net.WebSockets
open System.Collections.Generic
open System.Collections.Concurrent

open NetMQ
open NetMQ.Sockets

open Util.Cat
open Util.Bin
open Util.Text
open Util.Perf
open Util.Http
open Util.HttpServer
open Util.Concurrent

// RFC6455
// https://datatracker.ietf.org/doc/html/rfc6455
// https://blog.csdn.net/xmcy001122/article/details/117226953

// Data Framing

type OpCode =
| Continuation = 0
| Text = 1
| Bin = 2
| Close = 8
| Ping = 9
| Pong = 10

type Frame = {
fin: bool         
opCode: OpCode    
mask: bool       
payloadLen: int
maskKey: byte[]
payload: byte[] }

let parse (bytes:byte[]) = 

    let bits = bytes__bits bytes
    let mutable index = 0

    if bytes.Length >= 2 then

        let mask = bits[8]

        index <- index + 4       
        let opcode:OpCode = 
            (Array.sub bits index 8) >>>> 4
            |> bits__byte
            |> uint
            |> int
            |> EnumOfValue

        let payLoadLen = 
            index <- index + 4 + 1
            let payLoadLen = 
                (Array.sub bits index 8) >>>> 1
                |> bits__byte
                |> uint
                |> int

            if payLoadLen = 126 then
                index <- index + 16
                payLoadLen
            else if payLoadLen = 127 then
                index <- index + 64
                payLoadLen
            else
                payLoadLen

        let maskKey = 
            if mask && bits.Length >= index + 32 then
                let maskKey = Array.sub bits index 32
                index <- index  + 32
                maskKey |> bits__bytes
            else
                [||]

        {   fin = bits[0]
            opCode = opcode
            mask = mask
            payloadLen = payLoadLen
            maskKey = maskKey
            payload = [||] } |> Some
    else
        None



let wsDecode (bin:byte[]) =
    try
        let opcode = 
            match parse bin with
            | Some frame -> frame.opCode
            | None -> OpCode.Close

        // must be true, "All messages from the client to the server have this bit set"
        let mask = bin[1] &&& byte 0b10000000 <> byte 0
        let mutable offset = 2
        let mutable msglen = (bin[1] &&& byte 0b01111111) |> int32
        match msglen with
        | 126 -> 
            // bytes are reversed because websocket will print them in Big-Endian, whereas
            // BitConverter will want them arranged in little-endian on windows
            let a = [| bin[3]; bin[2] |]
            msglen <- int32 <| BitConverter.ToUInt16(a, 0)
            offset <- 4
        | 127 -> 
            // To test the below code, we need to manually buffer larger messages — since the NIC's autobuffering
            // may be too latency-friendly for this code to run (that is, we may have only some of the bytes in this
            // websocket frame available through client.Available).
            let a = [|bin[9]; bin[8]; bin[7]; bin[6]; bin[5]; bin[4]; bin[3]; bin[2]|]
            msglen <- int32(BitConverter.ToUInt64(a, 0))
            offset <- 10
        | _ -> ()
    
        if msglen = 0 then
            Some (opcode,[||])
        else
            if mask then
                let decoded = Array.zeroCreate msglen
                let masks = [| bin[offset]; bin[offset + 1]; bin[offset + 2]; bin[offset + 3] |]
                offset <- offset + 4
                let mutable i = 0
                while (i < msglen) && (i < bin.Length - offset) do
                    decoded[i] <- byte (bin[offset + i] ^^^ masks[i % 4])
                    i <- i + 1    
                Some (opcode,decoded)
            else
                None
    with ex -> None

let wsEncode (opcode:OpCode) (bin: byte[])  =
    let mutable frame = Array.zeroCreate 10    
    let mutable indexStartRawData = -1
    let mutable length = bin.Length
    
    frame[0] <- byte (128 + (EnumToValue opcode))
    if length <= 125 then
        frame[1] <- byte length
        indexStartRawData <- 2
    elif length >= 126 && length <= 65535 then
        frame[1] <- byte 126
        frame[2] <- byte ((length >>> 8) &&& 255)
        frame[3] <- byte (length &&& 255)
        indexStartRawData <- 4
    else    
        frame[1] <- byte 127
        frame[2] <- byte ((length >>> 56) &&& 255)
        frame[3] <- byte ((length >>> 48) &&& 255)
        frame[4] <- byte ((length >>> 40) &&& 255)
        frame[5] <- byte ((length >>> 32) &&& 255)
        frame[6] <- byte ((length >>> 24) &&& 255)
        frame[7] <- byte ((length >>> 16) &&& 255)
        frame[8] <- byte ((length >>> 8) &&& 255)
        frame[9] <- byte (length &&& 255)    
        indexStartRawData <- 10    
    
    let response = Array.zeroCreate (indexStartRawData + length)
    let mutable i = 0
    let mutable reponseIdx = 0
    //Add the frame bytes to the reponse
    while i < indexStartRawData do 
        response[reponseIdx] <- frame[i]
        i <- i + 1
        reponseIdx <- reponseIdx + 1    
    
    //Add the data bytes to the response
    i <- 0
    while i < length do 
        response[reponseIdx] <- bin[i]
        i <- i + 1
        reponseIdx <- reponseIdx + 1
    response
