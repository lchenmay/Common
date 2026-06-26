<template>

<div class="fst-container">
  <div v-if="loading" class="fst-loading">{{ t('loading') }}</div>
  <div v-else-if="nodes.length === 0" class="fst-empty">{{ t('empty') }}</div>
  <div v-else class="fst-list">
    <FileSysTreeNode
      v-for="node in nodes"
      :key="node.id"
      :node="node"
      :depth="0"
      :lang="props.lang"
      :baseUrl="props.baseUrl"
      @select="onSelect"
      @toggle="onToggle"
      @rename-folder="onRenameFolder"
      @download-zip="onDownloadZip"
    />
  </div>
</div>

</template>


<script setup lang="ts">

import { ref, onMounted } from 'vue'
import FileSysTreeNode from './FileSysTreeNode.vue'

// ---- Types ----

export interface FileNode {
  id: number
  name: string
  size: number
  type: 'folder' | 'file'
  suffix?: string
  children?: FileNode[]
  expanded?: boolean
}

// ---- Props ----

const props = defineProps<{
  lang?: string
  baseUrl?: string
  api?: string                // 获取树数据的 API
  nodes?: FileNode[]          // 直接传入节点数据（替代 API）
}>()

const emit = defineEmits<{
  (e: 'select', node: FileNode): void
  (e: 'rename-folder', node: FileNode): void
  (e: 'download-zip', node: FileNode): void
}>()

// ---- 多语言 ----

const t = (key: string): string => {
  const zh: Record<string, string> = {
    loading: '加载中...',
    empty: '无文件',
  }
  const en: Record<string, string> = {
    loading: 'Loading...',
    empty: 'No files',
  }
  if (props.lang === 'zh') return zh[key] || key
  return en[key] || key
}

// ---- State ----

const nodes = ref<FileNode[]>(props.nodes || [])
const loading = ref(false)

// ---- Methods ----

const onSelect = (node: FileNode) => {
  emit('select', node)
}

const onToggle = async (node: FileNode) => {
  node.expanded = !node.expanded
  // 如果展开了但 children 为空且是文件夹，尝试从 API 加载
  if (node.expanded && node.type === 'folder' && (!node.children || node.children.length === 0)) {
    if (props.api) {
      try {
        const rep = await fetch(`${props.api}?folderId=${node.id}`)
        const data = await rep.json()
        node.children = data.children || data
      } catch {}
    }
  }
}

const onRenameFolder = (node: FileNode) => {
  emit('rename-folder', node)
}

const onDownloadZip = (node: FileNode) => {
  emit('download-zip', node)
}

onMounted(async () => {
  if (props.nodes) {
    nodes.value = props.nodes
    return
  }
  if (props.api) {
    loading.value = true
    try {
      const rep = await fetch(props.api)
      const data = await rep.json()
      nodes.value = data.children || data
    } catch {}
    loading.value = false
  }
})

defineExpose({ nodes })

</script>


<style scoped>
.fst-container { font-size: 0.875rem; color: #334155; }
.fst-loading { padding: 1rem; color: #94a3b8; text-align: center; }
.fst-empty { padding: 1rem; color: #94a3b8; text-align: center; }
.fst-list { }
</style>
