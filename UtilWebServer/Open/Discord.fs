module Util.WebServer.Open.Discord

open System

open Util.Text
open Util.Json
open Util.HttpClient


let requestAccessToken(client_id,client_sceret,redirect_url) code = 

    let hc = empty__HttpClient()

    let postdata = 
        [|  "client_id=" + client_id
            "&client_secret=" + client_sceret
            "&grant_type=authorization_code"
            "&code=" + code;
            "&redirect_uri=" + redirect_url |]
        |> String.Concat

    let resobj = hc.post("https://discordapp.com/api/oauth2/token",postdata)
    let res = resobj.html
    let fields = 
        res |> jsonstr__items
        
    checkfield fields "access_token"

let requestUserInfo access_token = 

    let hc = empty__HttpClient()

    hc.insertions.Add("Authorization", "Bearer " + access_token)

    let json = hc.get("https://discordapp.com/api/users/@me").html

    let fields = 
        json
        |> jsonstr__items
        |> checkfield

    let uid = fields "id" |> try_parse_int64
    let username = fields "username"
    let discriminator = "discriminator"
    let mutable avatar = "avatar"

    // https://cdn.discordapp.com/avatars/614834018114076726/ffe7559e3fcfc1a729e9ddbcc779c6fd.png
    match uid with 
    | Some v ->
        if avatar.StartsWith "https://cdn.discordapp.com/avatars/" = false then
            avatar <- "https://cdn.discordapp.com/avatars/" + v.ToString() + "/" + avatar + ".png"
    | None -> ()

    uid, username + "#" + discriminator, avatar, json

