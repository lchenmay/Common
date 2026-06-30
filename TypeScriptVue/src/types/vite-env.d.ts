// src/types/vite-env.d.ts
/// <reference types="vite/client" />

declare module '*.vue' {
  import type { DefineComponent } from 'vue'
  const component: DefineComponent<{}, {}, any>
  export default component
}

// ✅ 扩展 ImportMeta 类型
interface ImportMeta {
  readonly env: {
    [key: string]: string | boolean | undefined
    readonly MODE: string
    readonly BASE_URL: string
    readonly PROD: boolean
    readonly DEV: boolean
    readonly SSR: boolean
  }
}