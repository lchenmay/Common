module UtilWebServer.Open.Discord

open System
open System.Threading
open System.Threading.Tasks

open Util.Text
open Util.Concurrent
open Util.Json
open Util.HttpClient

open Discord
open Discord.WebSocket

let requestAccessToken
    (client_id,client_sceret)
    redirect_url
    code = 

    let hc = empty__HttpClient()

    let postdata = 
        [|  ("client_id",client_id |> Json.Str)
            ("client_secret",client_sceret |> Json.Str)
            ("grant_type","authorization_code" |> Json.Str)
            ("redirect_uri",redirect_url |> Json.Str)
            ("code",code |> Json.Str)  |]
        |> Json.Braket
        |> json__strFinal

    let url = "https://discordapp.com/api/oauth2/token"

    let resobj = hc.post(url,postdata)
    resobj.html 
    |> str__root
    |> tryFindStrByAtt "access_token"

let requestUserInfo access_token = 

    let hc = empty__HttpClient()

    hc.insertions.Add("Authorization", "Bearer " + access_token)

    let json = hc.get("https://discordapp.com/api/users/@me").html

    let fields = 
        json
        |> jsonstr__items
        |> checkfield

    let uido = fields "id" |> try_parse_int64
    let username = fields "username"
    let mutable avatar = fields "avatar"

    // https://cdn.discordapp.com/avatars/614834018114076726/ffe7559e3fcfc1a729e9ddbcc779c6fd.png
    match uido with 
    | Some uid ->
        if avatar.StartsWith "https://cdn.discordapp.com/avatars/" = false then
            avatar <- "https://cdn.discordapp.com/avatars/" + uid.ToString() + "/" + avatar + ".png"
        
        (uid, username, avatar, json)
        |> Some
    | None -> None

let oauth2AuthCode(client_id,client_sceret,redirect_url) code = 

    let hc = empty__HttpClient()

    let postdata = 
        [|  "client_id=" + client_id
            "&client_secret=" + client_sceret
            "&grant_type=authorization_code"
            "&code=" + code
            "&redirect_uri=" + redirect_url |]
        |> linesConcat
    let resobj = hc.post("https://discordapp.com/api/oauth2/token",postdata)
    let res = resobj.html
    let fields = 
        res |> jsonstr__items
        
    (*
        {
            "access_token": "6qrZcUqja7812RVdnEKjpzOL4CvHBFG",
            "token_type": "Bearer",
            "expires_in": 604800,
            "refresh_token": "D43f5y0ahjqew82jZ4NViEr2YafMKhue",
            "scope": "identify"
        }     
        *)

    let access_token = Util.Json.checkfield(fields)("access_token")
    if access_token = "" then
        System.Console.WriteLine res
        printfn "%A" postdata
        printfn "%A" resobj.msg
        printfn "%A" resobj.returnHeaders
        printfn "%A" resobj.success
        printfn "%A" resobj.returnCode
    access_token

let oauth2UserInfo access_token = 

    let hc = empty__HttpClient()

    hc.insertions.Add("Authorization", "Bearer " + access_token)

    let json = hc.get("https://discordapp.com/api/users/@me").html;

    let fields = jsonstr__items json

    (*
        {
            "id": "614834018114076726",
            "username": "Gotterdammerung",
            "avatar": "ffe7559e3fcfc1a729e9ddbcc779c6fd",
            "discriminator": "4141",
            "public_flags": 0,
            "flags": 0,
            "banner": null,
            "banner_color": null,
            "accent_color": null,
            "locale": "en-US",
            "mfa_enabled": false
        }     
        *)

    let uid = Util.Json.checkfield(fields)("id") |> try_parse_int64
    let username = Util.Json.checkfield(fields)("username")
    let discriminator = Util.Json.checkfield(fields)("discriminator")
    let mutable avatar = Util.Json.checkfield(fields)("avatar")

    // https://cdn.discordapp.com/avatars/614834018114076726/ffe7559e3fcfc1a729e9ddbcc779c6fd.png
    if(uid.IsSome) then
        if(avatar.StartsWith("https://cdn.discordapp.com/avatars/") = false) then
            avatar <- "https://cdn.discordapp.com/avatars/" + uid.Value.ToString() + "/" + avatar + ".png"
    else System.Console.WriteLine json
    uid, username + "#" + discriminator, avatar, json


let checkDiscord (app,secret) (code,redirectUri) =
    let access_token = 
        oauth2AuthCode(app,secret,redirectUri) code
    oauth2UserInfo access_token

let token__client token = 

    let client = new DiscordSocketClient()

    (fun msg -> 
        ())
    |> fun__Func<SocketMessage>
    |> client.add_MessageReceived

    (fun _ -> 
        ())
    |> fun__FuncUnit
    |> client.add_Connected

    let t1 = client.LoginAsync(TokenType.Bot,token)
    let t2 = client.StartAsync()

    async{
        do! Async.AwaitTask t1
        do! Async.AwaitTask t2
    }|> Async.RunSynchronously

    while client.ConnectionState <> ConnectionState.Connected do
        System.Threading.Thread.Sleep 300

    client

let sendMsg (client:DiscordSocketClient) guildId channelId (content,embedding) = 

    let guild = client.GetGuild guildId
    let channel = guild.GetTextChannel channelId


    let eb = (new EmbedBuilder()).WithDescription embedding
    //await Context.Channel.SendMessageAsync("", false, eb.Build());


    let t = channel.SendMessageAsync(content,false,eb.Build())
    //let t = channel.SendMessageAsync content

    let mutable finished = false

    async{
        let! res = Async.AwaitTask t
        finished <- true
    }|> Async.RunSynchronously

    while finished = false do
        System.Threading.Thread.Sleep 300
