module Util.Browser

open System
open System.Collections.Generic
open System.IO
open System.IO.Compression
open System.Net
open System.Net.Security
open System.Net.Sockets
open System.Security.Cryptography.X509Certificates
open System.Security.Authentication
open System.Text
open System.Text.RegularExpressions

open Util.Bin
open Util.Perf
open Util.Text
open Util.Http
open Util.TcpServer



let CRLF = Util.Text.crlf

()