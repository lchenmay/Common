module Util.Tcp

open System
open System.IO
open System.Net
open System.Net.Security
open System.Net.Sockets
open System.Security.Cryptography.X509Certificates
open System.Security.Authentication

open Util
open Util.Bin



let bufferLength = 16 * 1024

type TcpState = 
| Closed = 0 // Initial
| Listen = 1
| SynSent = 2
| SynReceived = 3 
| Established = 4
| CloseWait = 5
| LastAck = 6
| FinWait1 = 7
| FinWait2 = 8
| Closing = 9
| TimeWait = 10

type TcpPacketArrivedEventArgs = 
    {
        headerLength: uint;
        protocal: string;
        ipVersion: string }

type TcpMsg = 
    {
        args: TcpPacketArrivedEventArgs;
            
        srcPort: int16;
        dstPort: int16;
        seq: int32;
        act: int32;
        dataOffset: int64;
        NS: bool;
        CWR: bool;
        ECE: bool;
        URG: bool;
        ACK: bool;
        PSH: bool;
        RST: bool;
        SYN: bool;
        FIN: bool;
        winSize: int16;
        checksum: int16;
        urgentPointer: int16;
        payload: byte[];

        dt: DateTime }

type TcpConn = 
    {
        srcIp: IPAddress;
        dstIp: IPAddress;
        srcPort: int16;
        dstPort: int16;
        msgs: Map<int32,TcpMsg> }


()