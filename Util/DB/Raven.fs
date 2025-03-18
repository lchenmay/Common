module Util.Raven

open LanguagePrimitives

open System
open System.Data
open System.Collections.Generic
open System.Security.Cryptography.X509Certificates
open System.IO
open System.Data
open System.Data.SqlClient
open System.Text

open Raven
open Raven.Client
open Raven.Client.Documents
open Raven.Client.ServerWide
open Raven.Client.Util

open Util.Perf
open Util.Cat
open Util.Collection
open Util.OS
open Util.Bin

let c (certFile:string) = new X509Certificate2(certFile)

let connect (certFile:string) url dbname = 

    let mutable store = 
        if certFile.Length > 0 then
            let cert = new X509Certificate2(certFile)
            new DocumentStore(Certificate = cert,Database = dbname, Urls = [| url |])
        else
            new DocumentStore(Database = dbname, Urls = [| url |])
    store.Initialize()

