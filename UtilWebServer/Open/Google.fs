module UtilWebServer.Open.Google

open System

open Util.Text
open Util.Json
open Util.HttpClient


let requestAccessToken(client_id:string[],client_sceret,redirect_url) code = 

        let hc = empty__HttpClient()

        let postdata = 
            [|  "client_id=" + client_id.[0];
                "&client_secret=" + client_sceret;
                "&grant_type=authorization_code";
                "&code=" + code;
                "&redirect_uri=" + redirect_url |]
            |> linesConcat

        let fields = 
            let res = hc.post("https://oauth2.googleapis.com/token",postdata).html
            res |> jsonstr__items

        let id_token = Util.Json.checkfield(fields)("id_token")
        //let access_token = Util.Json.checkfield(fields)("access_token")
        //https://www.googleapis.com/oauth2/v3/certs
        //access_token

        //Util.JWT.validate(id_token, "https://accounts.google.com", client_id))
        id_token    

//let requestUserInfo access_token = 

    //let verified,uid, email, avatar, fn, ln, name, lang, jwts, nickname,iss,aud,email_verified = 
    //    validate(id_token, "https://accounts.google.com", client_ids)
    //if verified = "verified" then
    //    uid, email, avatar, fn, ln, name, lang, jwts,uid
    //else
    //    "","","","","","","","",""

let translate apiKey src dst txt = 

    let hc = empty__HttpClient()
    
    let url = 
        [|  "https://translation.googleapis.com/language/translate/v2?key="
            apiKey |]
        |> String.Concat

    let postdata = 
        [|  "target=" + dst
            "&q=" + Util.Json.encode(txt) |]
        |> String.Concat

    let html = hc.post(url,postdata).html

    let mutable res = ""

    let mutable json = html |> Util.Json.str__root
    match tryFindByPath [| "data";"translations" |] json with
    | Some (n,j) -> 
        match j with
        | Json.Ary items -> 
            if items.Length = 1 then
                res <- 
                    tryFindStrByAtt "translatedText" items[0]
                    |> Util.Json.decode
        | _ -> ()
    | None -> ()

    (*
{
  "data": {
    "translations": [
      {
        "translatedText": "Chinese translation test",
        "detectedSourceLanguage": "zh-CN"
      }
    ]
  }
}
    
    *)

    res