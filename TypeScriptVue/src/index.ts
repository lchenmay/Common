// src/index.ts

// ============================================
// 导出所有 Vue 组件
// ============================================
export { default as Crud } from './comps/Crud.vue'
export { default as TabContainer } from './comps/TabContainer.vue'
export { default as TablePaged } from './comps/TablePaged.vue'
export { default as SearchField } from './comps/SearchField.vue'
export { default as Markdown } from './comps/Markdown.vue'

// ============================================
// 导出所有工具函数
// ============================================
export * from './lib/text'
export * from './lib/api'

// 如果 lib 下有 index.ts，可以统一导出
// export * from './lib'