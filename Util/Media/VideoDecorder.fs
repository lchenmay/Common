module Util.VideoDecorder

open System
open System.IO
open System.Text.Json
open System.Diagnostics

open Util.Cat

let (>>=) c f = 
    match c with
    | Result.Ok x -> f x
    | Result.Error e -> Result.Error e


let (>=>) f g x = 
    f x >>= g


type MediaEncoderError = {
    src: string
    dst: string
    error: string }

type AudioVideo = AudioStreamQuery | VideoStreamQuery
type MediaInfoError = MediaInfoError of string
type Video = double * int * int //{ durationSec: double; width: int; height: int }
type Dim = int * int 

type ProcessTweaker = {
    preStart : Process -> unit
    postStart: Process -> unit }

type VideoStreamDto = {|
    codec_type: string
    width: int
    height: int
    duration: string
|}

type StreamsDto<'streamDto> = {|
    streams: 'streamDto array
|}

let notEmpty = System.String.IsNullOrEmpty >> not
let emptyTweaker = { preStart = ignore; postStart = ignore }
let preStartTweaker preStart = { emptyTweaker with preStart = preStart }
let postStartTweaker postStart = { emptyTweaker with postStart = postStart }

let video__dim videoStream = 
    let durationSec, width, height = videoStream
    width,height

let mapMediaInfoError<'a> : Result<'a, MediaInfoError> -> Result<'a, string> =
    Result.mapError (function MediaInfoError e -> sprintf "[MediaInfoError]: %s" e)

let mapMediaEncoderError<'a> : Result<'a, MediaEncoderError> -> Result<'a, string> =
    Result.mapError 
        (fun e ->
            sprintf "[MediaEncoderError]: Source file: [ %s ] Target file: [ %s ] Error: %s"
                e.src e.dst e.error)

let VideoStreamDto__Video (dto: VideoStreamDto) =
    if dto.codec_type = "video" then
        Ok(Util.Text.parse_float dto.duration,dto.width,dto.height)
    else
        dto
        |> sprintf "Not the video stream: %A"
        |> Error

let deserialize<'a> (s: string): Result<'a, string> =
    try
        s
        |> JsonSerializer.Deserialize<'a>
        |> Ok
    with
    e -> e.Message |> sprintf "Deserialization failed with: %s" |> Error

let deserialize'<'a> =
    deserialize<'a>
    >> Result.mapError (sprintf "ffprobe deserialization failed with: %s")

let createProcess (startInfo: ProcessStartInfo) : Process =
    let process' = new Process()
    process'.StartInfo <- startInfo
    process'

let getStartInfo (cmd:string) (args:string) =
    let startInfo = ProcessStartInfo(cmd, args)
    startInfo.CreateNoWindow <- true
    startInfo.RedirectStandardOutput <- true
    startInfo.RedirectStandardError <- true
    startInfo
    

let runProcessSuccessfully processTweaker (startInfo: ProcessStartInfo) : Result<Process, string> =        
    try
        let process' = createProcess startInfo
        processTweaker.preStart process'
        process'.Start() |> ignore
        processTweaker.postStart process'
        process'.WaitForExit()

        if process'.ExitCode <> 0 then
            let stdErr = process'.StandardError.ReadToEnd()
                
            if notEmpty stdErr 
            then sprintf "Process error %i: %s" process'.ExitCode stdErr 
            else sprintf "Unknown process error %i" process'.ExitCode
            |> Error            
        else
            Ok process'
    with
    | ex ->
        ex.Message
        |> sprintf "Process [ %s %s ] failed with: %s" startInfo.FileName startInfo.Arguments
        |> Error

let runProcessSuccessfully' processTweaker cmd args =
    getStartInfo cmd args
    |> runProcessSuccessfully processTweaker

let getProcessOutput (process': Process) : Result<string, string> =
    try
        process'.StandardOutput.ReadToEnd()
        |> Ok
    with
    | e ->
        e.Message
        |> sprintf "Output retrieval failed for process [ %s %s ] with: %s"
                process'.StartInfo.FileName
                process'.StartInfo.Arguments
        |> Error

let runAndGetProcessOutput processTweaker cmd args : Result<string, string> =
    let a = 
        getStartInfo cmd args
        |> runProcessSuccessfully processTweaker
    a >>= getProcessOutput

let runAndGetNonEmptyProcessOutput processTweaker cmd args : Result<string, string> =
    runAndGetProcessOutput processTweaker cmd args
    >>= (fun s -> if notEmpty s then Ok s else Error "Empty process output.")

let run_ffprobe_query ffprobeQuery : Result<string, string> =
    runAndGetNonEmptyProcessOutput
        emptyTweaker
        "ffprobe"
        ffprobeQuery

let ffprobe_query queryKind filePath =
    let streamSpecifier = 
        match queryKind with
        | AudioStreamQuery -> "a:0"
        | VideoStreamQuery -> "v:0"
            
    // ffprobe -hide_banner -loglevel 0 -print_format json -show_streams -select_streams v:0 "/Users/mac/Downloads/1.avi"
    sprintf "-hide_banner -loglevel error -print_format json -show_streams -select_streams %s \"%s\""
        streamSpecifier
        filePath

let getFirstStream<'a> (x : StreamsDto<'a>) =
    if x.streams.Length <> 0 then
        Ok x.streams[0]
    else
        x
        |> sprintf "'%s' array is empty: %A" (nameof(x.streams))
        |> Error

let getStream<'streamDto> queryKind =
    ffprobe_query queryKind
    >> run_ffprobe_query
    >=> deserialize'<StreamsDto<'streamDto>>
    >=> getFirstStream

let getVideoStream : string -> Result<Video, string> =
    getStream<VideoStreamDto> VideoStreamQuery
    >=> VideoStreamDto__Video

let isNonEmptyFile filePath : Result<bool, string> =
    try
        let fileInfo = FileInfo(filePath)
        Ok ( fileInfo.Extension=".m3u8" || fileInfo.Exists && fileInfo.Length > 0)
    with
    | e -> e.Message |> sprintf "Non-emptiness check failed for file [ %s ] with error: %s" filePath |> Error

let fileExists filePath : Result<string, string> = result {
    let! fileIsNonEmpty = isNonEmptyFile filePath
    return! if fileIsNonEmpty
        then Ok filePath 
        else filePath |> sprintf "File [ %s ] is absent or empty" |> Error
}

let getDimensions' =
    fileExists
    >=> getVideoStream
    >> Result.map video__dim

let getDimensions : string -> Result<Dim, MediaInfoError> =        
    getDimensions'
    >> Result.mapError MediaInfoError

let ffmpeg_run_query ffmpegQuery =

    runProcessSuccessfully'
        {
            preStart = (fun process' ->
                process'.StartInfo.RedirectStandardOutput <- false
                process'.StartInfo.RedirectStandardError <- false
            )
            postStart = (fun process' -> 
                process'.PriorityClass <- ProcessPriorityClass.BelowNormal
            )
        }
            
        "ffmpeg"
        ffmpegQuery
    |> Result.map ignore

// ffmpeg -i "/Users/mac/Downloads/1.avi" -loglevel fatal -y -c:v libx264 -crf 24 -pix_fmt yuv420p -tune film -c:a aac -b:a 192k -ar 44100  -strict -2 -speed fastest -s 1920x1080 /User/mac/Downloads/1.mp4
// -movflags faststart --- 把 moov 信息排列到 mp4 文件头, 在网络播放视频时, 缩短100-200ms寻找视频头的时间
let ffmpeg_encode_query sourceFilePath targetFilePath dim =
    let w,h = dim
    $"-i \"{sourceFilePath}\" -loglevel fatal -y -c:v libx264 -crf 24 -pix_fmt yuv420p  -movflags faststart -tune film -c:a aac -b:a 192k -ar 44100  -strict -2 -speed fastest -s {w}x{h} \"{targetFilePath}\""

let getErrorDetails sourceFilePath targetFilePath error = 
    {
        src = sourceFilePath
        dst = targetFilePath
        error = error }

let encodeFile src dst dim =
    ffmpeg_encode_query src dst dim
    |> ffmpeg_run_query 
    |> Result.map (fun () -> dst)
    |> Result.mapError (getErrorDetails src dst)

type HandleOutcome = Encoded | Skipped

let handleStream src  dst= result {
    let! dim = getDimensions src |> mapMediaInfoError
    let! encodedFilePath = encodeFile src dst dim |> mapMediaEncoderError
    return Encoded
}
let isMediaReady src = result {
    let! dim = getDimensions src |> mapMediaInfoError
    return dim
}

// mkv avi mov wmv mp4 webm mpeg m4v-> mp4
// let sourceFilePath = "/Users/mac/Downloads/1.avi"
// let targetFilePath = "/Users/mac/Downloads/4.mp4"
// let res1 = handleStream sourceFilePath targetFilePath

// m3u8 -> mp4
// let sourceFilePath = "https://media.gettr.com/group5/getter/2023/02/21/15/af2b0f9a-28e1-89bd-43e2-30a01c6914bf/720p_v.m3u8"
// let targetFilePath = "/Users/mac/Downloads/5.mp4"
// let res2 = handleStream sourceFilePath targetFilePath