module UtilKestrel.BashDeployer.Thumbnail

open System
open System.Diagnostics
open System.Runtime.InteropServices


let checkGhostscript (output: string->unit) =
    let checkCommand (cmd: string) (args: string) =
        try
            let startInfo = ProcessStartInfo()
            startInfo.FileName <- cmd
            startInfo.Arguments <- args
            startInfo.UseShellExecute <- false
            startInfo.RedirectStandardOutput <- true
            startInfo.RedirectStandardError <- true
            startInfo.CreateNoWindow <- true

            use proc = new Process()
            proc.StartInfo <- startInfo
            proc.Start() |> ignore
            proc.WaitForExit(5000) |> ignore

            if proc.ExitCode = 0 then
                let version = proc.StandardOutput.ReadToEnd().Trim()
                sprintf "[%s] Found: %s (version: %s)" cmd cmd version |> output
                true
            else
                false
        with _ -> false

    let checkImageMagick (output: string->unit) =
        let commands =
            if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then
                ["magick.exe"; "convert.exe"]
            else
                ["magick"; "convert"]

        let found = commands |> List.tryFind (fun cmd -> checkCommand cmd "--version")

        match found with
        | Some cmd ->
            sprintf "[ImageMagick] Found: %s" cmd |> output
            true
        | None ->
            "[ImageMagick] NOT FOUND" |> output
            if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then
                "[ImageMagick] Windows installation:" |> output
                "[ImageMagick] 1. Download: https://imagemagick.org/script/download.php#windows" |> output
                "[ImageMagick] 2. Install and add to PATH" |> output
                "[ImageMagick] Or use Chocolatey: choco install imagemagick" |> output
            elif RuntimeInformation.IsOSPlatform(OSPlatform.Linux) then
                "[ImageMagick] Linux installation:" |> output
                "[ImageMagick] Ubuntu/Debian: sudo apt install imagemagick" |> output
                "[ImageMagick] CentOS/RHEL: sudo yum install ImageMagick" |> output
            elif RuntimeInformation.IsOSPlatform(OSPlatform.OSX) then
                "[ImageMagick] macOS installation:" |> output
                "[ImageMagick] brew install imagemagick" |> output

            "[ImageMagick] Impact: PDF thumbnails will NOT be generated" |> output
            false

    let checkGhostscriptInternal (output: string->unit) =
        let commands =
            if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then
                ["gswin64c.exe"; "gswin32c.exe"; "gs.exe"]
            else
                ["gs"]

        let found = commands |> List.tryFind (fun cmd -> checkCommand cmd "--version")

        match found with
        | Some cmd -> true
        | None ->
            "[Ghostscript] NOT FOUND" |> output
            if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then
                "[Ghostscript] Windows installation:" |> output
                "[Ghostscript] 1. Download: https://www.ghostscript.com/releases/gsdnld.html" |> output
                "[Ghostscript] 2. Install and add to PATH" |> output
                "[Ghostscript] Or use Chocolatey: choco install ghostscript" |> output
            elif RuntimeInformation.IsOSPlatform(OSPlatform.Linux) then
                "[Ghostscript] Linux installation:" |> output
                "[Ghostscript] Ubuntu/Debian: sudo apt install ghostscript" |> output
                "[Ghostscript] CentOS/RHEL: sudo yum install ghostscript" |> output
            elif RuntimeInformation.IsOSPlatform(OSPlatform.OSX) then
                "[Ghostscript] macOS installation:" |> output
                "[Ghostscript] brew install ghostscript" |> output

            "[Ghostscript] Impact: PDF thumbnails will NOT be generated" |> output
            false

    "检查 Ghostscript 环境（PDF 缩略图生成依赖）" |> output
    let gsOk = checkGhostscriptInternal output
    "检查 ImageMagick 环境（PDF 缩略图生成依赖）" |> output
    let imOk = checkImageMagick output

    gsOk && imOk

