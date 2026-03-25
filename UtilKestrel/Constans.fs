module UtilWebServer.Constants

open System
open System.Text
open System.IO
open System.Diagnostics

let en = "en"
let zh = "zh"
let jp = "jp"

let freqLangCodes = 
    [|  en
        zh
        "fr"
        "de"
        "jp"
        "it" |]

let freqCurCodes = 
    [|  "USD"
        "EUR"
        "GBP"
        "CHF"
        "JPY"
        "CAD"
        "AUD"
        "NZD"
        "TRY"
        "MXN"
        "ZAR"
        "BTC"
        "ETH"
        "CNY" |]

let freqBizCodes = 
    [|  "X"
        "GOOGLE"
        "FACEBOOK"
        "INSTAGRAM"
        "PINTEREST"
        "COINGECKO"
        "COINDESK"
        "CRYPTOSLATE"
        "COINTELEGRAPH"
        "BLOCKCHAIN.NEWS"
        "DISCORD"
        "DECRYPT.CO"
        "GAB"
        "GETTR"
        "GNEWS" |]
