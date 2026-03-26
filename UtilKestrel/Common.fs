module UtilKestrel.Common

open System
open System.IO
open System.Net
open System.Net.Sockets
open System.Collections.Concurrent
open System.Text
open System.IO
open System.Diagnostics

open Util.Cat
open Util.CollectionModDict
open Util.Console
open Util.Json
open Util.Http

let since = DateTime.UtcNow

let output:string -> unit = 

    let assbly = System.Reflection.Assembly.GetCallingAssembly()
    let dir = Directory.GetCurrentDirectory()
    if dir.EndsWith "WebService" then
        prompt since >> Debug.WriteLine
    else if dir.EndsWith "WebApp" then
        prompt since >> Debug.WriteLine
    else
        Console.OutputEncoding <- Encoding.Unicode
        prompt since >> Console.WriteLine
