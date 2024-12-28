module Util.SixLaborsImageSharp

open System.IO

open SixLabors.ImageSharp
open SixLabors.ImageSharp.Processing
open SixLabors.ImageSharp.Formats.Jpeg

let generateThumbnail 
    (w:int,h:int)
    (input:byte[]) = 

    use image = Image.Load (new MemoryStream(input))

    image.Mutate(fun ctx -> ctx.Resize(w,h) |> ignore)

    let output = new MemoryStream()
    image.SaveAsPng(output)

    output.ToArray()
    

