// src/types/markdown-it-katex.d.ts
declare module 'markdown-it-katex' {
  import { PluginWithOptions } from 'markdown-it'
  
  interface KatexOptions {
    throwOnError?: boolean
    errorColor?: string
    macros?: Record<string, string>
    displayMode?: boolean
    trust?: boolean
  }
  
  const markdownItKatex: PluginWithOptions<KatexOptions>
  export default markdownItKatex
}