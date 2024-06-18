module UtilWebServer.Auth3rdParty

open System
open System.Text
open System.IO
open System.Diagnostics
open System.Collections.Generic
open System.Collections.Concurrent

open Util.Cat
open Util.Perf
open Util.Crypto
open Util.DbQuery
open Util.DbTx
open Util.Text
open Util.Json
open Util.Http
open Util.HttpClient
open Util.Orm
open Util.Db
open Util.Zmq

//let validate (token: string , issuer: string, audiences: string[]) =
//    let a = 
//        (token+".").Split "."
//    let header =
//        let b64txt = a[0] |> decode64urlstring
//        b64txt |> jsonstr__items
//    let kid = checkfield(header)("kid")

////        if(iss = issuer && kid <> "" && ( (List.contains aud (List.ofArray(audiences))) || (List.contains azp (List.ofArray(audiences))) ) ) then
//    if(kid <> "") then
//        let k_serialized = get_publickey issuer kid
//        if(k_serialized <> "") then
//            let k = JsonWebKey(k_serialized)
//            let para = TokenValidationParameters(
//                ValidateIssuer = false,
//                ValidateLifetime = true,
//                ValidateAudience = false,
//                IssuerSigningKey = k // alternatively, rsask can be used            
//            )         

//            let tokenHandler = JwtSecurityTokenHandler()
//            let t = JwtSecurityToken()
//            try
//                let (c,tok) = tokenHandler.ValidateToken(token , para)
//                let p = (tok :?> JwtSecurityToken).Payload
//                let iss =
//                    if p.ContainsKey("iss") then
//                            p.["iss"].ToString()
//                    else ""
//                let aud = 
//                    if p.ContainsKey("aud") then
//                            p.["aud"].ToString()
//                    else ""
//                let azp = 
//                    if p.ContainsKey("azp") then
//                            p.["azp"].ToString()
//                    else ""
//                let uid = 
//                    if p.ContainsKey("sub") then
//                            p.["sub"].ToString()
//                    else ""
//                let email = 
//                    if p.ContainsKey("email") then
//                            p.["email"].ToString()
//                    else ""
//                let avatar = 
//                    if p.ContainsKey("picture") then
//                            p.["picture"].ToString()
//                    else ""
//                let name = 
//                    if p.ContainsKey("name") then
//                            p.["name"].ToString()
//                    else ""
//                let nickname = 
//                    if p.ContainsKey("nickname") then
//                            p.["nickname"].ToString()
//                    else ""
//                let fn = 
//                    if p.ContainsKey("given_name") then
//                            p.["given_name"].ToString()
//                    else ""
//                let ln = 
//                    if p.ContainsKey("family_name") then
//                            p.["family_name"].ToString()
//                    else ""
//                let lang = 
//                    if p.ContainsKey("locale") then
//                            p.["locale"].ToString()
//                    else ""
//                let email_verified = 
//                    if p.ContainsKey("email_verified") then
//                            p.["email_verified"].ToString()
//                    else ""
//                if iss = issuer && ( (List.contains aud (List.ofArray(audiences))) || (List.contains azp (List.ofArray(audiences))) ) then
//                    "verified",uid, email, avatar, fn, ln, name, lang, a[1], nickname,iss,aud,email_verified
//                else "iss/aud",uid, email, avatar, fn, ln, name, lang, a[1], nickname,iss,aud,email_verified
//            with
//                ex -> "unverified","", "", "", "", "", "", "", a[1], "","","",""
//        else "cert","", "", "", "", "", "", "", a[1], "","","",""
//    else "kid","", "", "", "", "", "", "", a[1], "","","",""

//let oauth2AuthCode(client_id:string[],client_sceret,redirect_url)(code) = 

//    let hc = empty__HttpClient()

//    let postdata = 
//        [|  "client_id=" + client_id.[0];
//            "&client_secret=" + client_sceret;
//            "&grant_type=authorization_code";
//            "&code=" + code;
//            "&redirect_uri=" + redirect_url |]
//        |> linesConcat

//    let fields = 
//        let res = hc.post("https://oauth2.googleapis.com/token",postdata).html
//        res |> jsonstr__items

//    (*
//        {
//            "access_token": "6qrZcUqja7812RVdnEKjpzOL4CvHBFG",
//            "token_type": "Bearer",
//            "expires_in": 604800,
//            "refresh_token": "D43f5y0ahjqew82jZ4NViEr2YafMKhue",
//            "scope": "identify"
//        }     
//        *)

//    let id_token = Util.Json.checkfield(fields)("id_token")
//    //let access_token = Util.Json.checkfield(fields)("access_token")
//    //https://www.googleapis.com/oauth2/v3/certs
//    //access_token

//    //Util.JWT.validate(id_token, "https://accounts.google.com", client_id))
//    id_token    
 
//let idToken2UserInfo(id_token,client_ids:string[]) =

//    let verified,uid, email, avatar, fn, ln, name, lang, jwts, nickname,iss,aud,email_verified = 
//        validate(id_token, "https://accounts.google.com", client_ids)
//    if verified = "verified" then
//        uid, email, avatar, fn, ln, name, lang, jwts,uid
//    else
//        "","","","","","","","",""

//type GoogleGeoPlace = 
//    {
//        results: Array }


//let text__geoo s = 
        
//    let url = 
//        [|  "https://maps.googleapis.com/maps/api/geocode/json?address=";
//            s;
//            "&key=AIzaSyDO-IEZxbzXVaLAFirayyMO2q66w9yVwTQ"  |]
//            //"&key=AIzaSyCO-hySDa94eIFPnbzwqu9jOs1dys6F_nY"  |]
//        |> linesConcat

//    let a,html = Util.HttpClient.httpGet None (url)
//    if(a = "OK" && html.Contains("\x22status\x22 : \x22ZERO_RESULTS\x22") = false) then

//        let a = 
//            (html |> regex_match(string__regex("\x22location\x22.*?}"))).Replace(":","")

//        let lng = 
//            (a 
//            |> regex_match(string__regex("(?<=\x22lng\x22)[^,]+"))
//            |> regex_match(string__regex("[\d\.-]+"))).Trim()
//            |> parse_float

//        let lat = 
//            (a 
//            |> regex_match(string__regex("(?<=\x22lat\x22)[^,]+"))
//            |> regex_match(string__regex("[\d\.-]+"))).Trim()
//            |> parse_float

//        if(lng * lng + lat * lat > 0.0) then
//            Some(lng,lat,html)
//        else
//            None
//    else
//        None

//// Firebase functions
//let pushAppNotification(title:string, message:string, id:string, topic:string) = 
//    let fcmUrl = "https://fcm.googleapis.com/fcm/send"
//    let header = Dictionary<string, string>(dict [ ("Authorization", "key=" + runtime.host.firebaseApiKey); ])
//    let sb = new StringBuilder()
//    sb.Append("{\"to\":\"/topics/" + topic + "\",") |> ignore
//    sb.Append("\"notification\":{\"title\" :\"" + title + "\",\"body\":\"" + message + "\"},") |> ignore
//    sb.Append("\"data\":{\"url\" :\"" + Runtime.runtime.host.websiteUrl + "/articles?action=breaking&param={\\\"tid\\\":\\\"" + id + "\\\"}\"}}") |> ignore
//    let postData = sb.ToString()
//    let res, msg = Util.HttpClient.httpPostWithHeadera(fcmUrl, postData, header) |> Async.RunSynchronously
//    if (res <> "OK") then 
//        msg.ToString()
//        |> exn__logNoSql "WebInterop.Google.pushAppNotification"

//let validateCapcha (x:X) response =
//    let hc = empty__HttpClient()
//    let url = "https://www.google.com/recaptcha/api/siteverify"
//    let postdata = 
//        [|  "secret=" + x.runtime.host.reCapthchaKey;
//            "&response=" + response; |]
//        |> linesConcat
//    let res = hc.post(url,postdata).html
//    let fields = jsonstr__items res
//    if (checkfield(fields)("success")) = "true" then
//        true
//    else false






