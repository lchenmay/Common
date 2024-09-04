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

(*

Loading UserInfo via Discord OAuth

1. Setup Discord App with:

a) client_id = app_id
b) client_secret
c) redirect_url
d) permission scope: identity
e) generated_url from c) and d)

2. End user visit:

f) generated_url e) from browser an
g) Discord redirects the web page to the redirect_url c) with param code=xxx
h) Obtain code=xxx

Call sample:

let code = "m02dtmoYgEe6UNuB9P3CZ9IqfyfBoA"

let accessToken =
    UtilWebServer.Open.Discord.requestAccessToken
        (runtime.host.DiscordAppId,runtime.host.DiscordSecret)
        runtime.host.DiscordRedirect
        code

let res = 
    UtilWebServer.Open.Discord.requestUserInfo accessToken

*)

let requestAccessToken
    (client_id,client_sceret)
    redirect_url
    code = 

    let urlRequestAuthCode = "https://discordapp.com/api/oauth2/token"

    let hc = Util.HttpClient.empty__HttpClient()
    let postdata = 
        [|  "client_id=" + client_id
            "&client_secret=" + client_sceret
            "&grant_type=authorization_code"
            "&redirect_uri=" + redirect_url
            "&code=" + code  |]
        |> String.Concat
    let json = hc.post(urlRequestAuthCode,postdata).html
    let code = 
        json 
        |> str__root
        |> tryFindStrByAtt "access_token"
    code,json

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

    try
        let guild = client.GetGuild guildId
        let channel = guild.GetTextChannel channelId


        let eb = (new EmbedBuilder()).WithDescription embedding
        eb.Color <- new Color(0x24,0xEB,0x1F)
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
    with
    | _ -> ()