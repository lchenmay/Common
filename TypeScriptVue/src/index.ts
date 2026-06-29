// src/index.ts

// ============================================
// 导出所有 Vue 组件
// ============================================
export { default as Crud } from './comps/Crud.vue'
export { default as TabContainer } from './comps/TabContainer.vue'
export { default as TablePaged } from './comps/TablePaged.vue'
export { default as SearchField } from './comps/SearchField.vue'
export { default as Markdown } from './comps/Markdown.vue'
export { default as FileUploader } from './comps/FileUploader.vue'
export { default as Thumbnail } from './comps/Thumbnail.vue'
export { default as FileSysTree } from './comps/FileSysTree.vue'
export { default as BatchUploader } from './comps/BatchUploader.vue'
export type { UploadTask, UploaderProps } from './comps/BatchUploader.vue'

// ============================================
// 导出 Theme 类型
// ============================================
export type { Theme } from './lib/util/theme'

// ============================================
// 导出 lib 层 —— 工具函数
// ============================================

// --- 基础工具 ---
export * from './lib/util/misc'
export * from './lib/util/text'

// --- 二进制序列化 ---
export * from './lib/util/bin'

// --- HTTP / 网络 ---
export { checkUrl, upload, loader } from './lib/api'
export { post, get } from './lib/util/fetch'

// --- WebSocket ---
export { createWebSocket_base, disconnect, trySend, trySendx } from './lib/util/ws'

// --- 图形 (Graphics) ---
export * as Graphics from './lib/util/graphics'
export * as GraphicsH5 from './lib/util/graphicsH5'
export * as GraphicsPixi from './lib/util/graphicsPixi'

// --- Markdown ---
export { markdown__html, markdown__html_, html__toc } from './lib/util/markdown'

// --- 认证 (Auth) ---
export { SignOut, biz__LoginOptions, LoginOption__RT, host__DiscordRedirectURL } from './lib/mod/auth'

// --- 通用 (Common) ---
export { buildHost, getLocalStorage, setLocalStorage, checkDomain } from './lib/common'

// --- jlib 工厂函数 (泛型项目聚合器) ---
export { createJlib } from './lib/jlib'

// --- notify / panel (通用模块) ---
export * from './lib/mod/notify'
export * from './lib/mod/panel'

// --- route 工厂函数 (泛型路由，参数化 routes) ---
export { createProjectRouter } from './lib/mod/route'