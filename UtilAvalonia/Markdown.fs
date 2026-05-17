module UtilAvalonia.Markdown

open System
open System.Text.RegularExpressions
open Avalonia
open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Media

/// Markdown 渲染配置
type MarkdownRenderOptions = {
    FontSize: float
    CodeFontSize: float
    LinkColor: Color
    CodeBackground: Color
}

let defaultOptions = {
    FontSize = 12.0
    CodeFontSize = 11.0
    LinkColor = Color.Parse("#569CD6")
    CodeBackground = Color.Parse("#1E1E1E")
}

/// 渲染 Markdown 文本为 StackPanel
let renderMarkdown (text: string) (options: MarkdownRenderOptions) =
    let stackPanel = StackPanel()
    stackPanel.Spacing <- 5.0
    
    let lines = text.Split('\n')
    let mutable inCodeBlock = false
    let mutable codeContent = ResizeArray<string>()
    
    let flushCodeBlock () =
        if codeContent.Count > 0 then
            let codeText = String.Join("\n", codeContent)
            let codeBorder = Border(
                Background = SolidColorBrush(options.CodeBackground),
                CornerRadius = CornerRadius(4.0),
                Margin = Thickness(0.0, 2.0, 0.0, 2.0),
                Padding = Thickness(8.0, 4.0)
            )
            let codeBlock = TextBlock(
                Text = codeText,
                FontSize = options.CodeFontSize,
                FontFamily = FontFamily("Consolas, 'Courier New', monospace"),
                TextWrapping = TextWrapping.Wrap
            )
            codeBorder.Child <- codeBlock
            stackPanel.Children.Add(codeBorder) |> ignore
            codeContent.Clear()
    
    for line in lines do
        let trimmed = line.Trim()
        
        // 处理代码块
        if trimmed.StartsWith("```") then
            if inCodeBlock then
                flushCodeBlock()
                inCodeBlock <- false
            else
                flushCodeBlock()
                inCodeBlock <- true
        elif inCodeBlock then
            codeContent.Add(line)
        else
            let textBlock = TextBlock()
            textBlock.TextWrapping <- TextWrapping.Wrap
            textBlock.Margin <- Thickness(0.0, 2.0, 0.0, 2.0)
            textBlock.FontSize <- options.FontSize
            
            if line.StartsWith("# ") then
                textBlock.Text <- line.Substring(2)
                textBlock.FontSize <- options.FontSize + 6.0
                textBlock.FontWeight <- FontWeight.Bold
            elif line.StartsWith("## ") then
                textBlock.Text <- line.Substring(3)
                textBlock.FontSize <- options.FontSize + 4.0
                textBlock.FontWeight <- FontWeight.Bold
            elif line.StartsWith("### ") then
                textBlock.Text <- line.Substring(4)
                textBlock.FontSize <- options.FontSize + 2.0
                textBlock.FontWeight <- FontWeight.Bold
            elif line.StartsWith("- ") || line.StartsWith("* ") then
                textBlock.Text <- "• " + line.Substring(2)
            elif line.StartsWith("> ") then
                textBlock.Text <- line.Substring(2)
                textBlock.Foreground <- SolidColorBrush(Color.Parse("#6A9955"))
                textBlock.FontStyle <- FontStyle.Italic
            elif trimmed.StartsWith("```") |> not then
                // 处理行内格式
                let processed = 
                    Regex.Replace(line, @"\*\*(.+?)\*\*", "$1")
                textBlock.Text <- processed
                if line.Contains("**") then
                    textBlock.FontWeight <- FontWeight.Bold
                // 处理行内代码
                if line.Contains("`") then
                    textBlock.Text <- Regex.Replace(processed, @"`(.+?)`", "$1")
                    textBlock.FontFamily <- FontFamily("Consolas, 'Courier New', monospace")
            
            if textBlock.Text <> null && textBlock.Text <> "" then
                stackPanel.Children.Add(textBlock) |> ignore
    
    flushCodeBlock()
    stackPanel

/// 获取预览文本（最多两行）
let getPreviewText (text: string) maxLines =
    let lines = text.Split('\n')
    if lines.Length <= maxLines then text
    else 
        let previewLines = lines |> Array.take maxLines
        String.Join("\n", previewLines) + "\n..."