<template>

<div class="fstn-wrapper">
  <!-- 行 -->
  <div
    class="fstn-row"
    :class="{ 'fstn-selected': selected }"
    :style="{ paddingLeft: (depth * 20 + 8) + 'px' }"
    @click="onClick"
  >
    <!-- 展开/收起箭头 -->
    <span v-if="node.type === 'folder'" class="fstn-arrow" @click.stop="onToggle">
      {{ node.expanded ? '▼' : '▶' }}
    </span>
    <span v-else class="fstn-arrow-placeholder"></span>

    <!-- 图标 -->
    <Thumbnail
      v-if="node.type === 'file'"
      :name="node.name"
      :fileId="node.id"
      :baseUrl="baseUrl"
      class="fstn-icon"
    />
    <span v-else class="fstn-folder-icon">
      {{ node.expanded ? '📂' : '📁' }}
    </span>

    <!-- 文件名 -->
    <span class="fstn-name" :title="node.name">{{ node.name }}</span>

    <!-- 文件大小 -->
    <span v-if="node.type === 'file'" class="fstn-size">{{ formatSize(node.size) }}</span>
    <span v-else class="fstn-count">{{ childCount }}</span>
  </div>

  <!-- 子节点 -->
  <div v-if="node.type === 'folder' && node.expanded && node.children" class="fstn-children">
    <FileSysTreeNode
      v-for="child in node.children"
      :key="child.id"
      :node="child"
      :depth="depth + 1"
      :lang="lang"
      :baseUrl="baseUrl"
      @select="onChildSelect"
      @toggle="onChildToggle"
    />
  </div>
</div>

</template>


<script setup lang="ts">

import { ref, computed } from 'vue'
import Thumbnail from './Thumbnail.vue'
import { formatSize } from '../lib/util/misc'
import type { FileNode } from './FileSysTree.vue'

// ---- Props ----

const props = defineProps<{
  node: FileNode
  depth: number
  lang?: string
  baseUrl?: string
}>()

const emit = defineEmits<{
  (e: 'select', node: FileNode): void
  (e: 'toggle', node: FileNode): void
}>()

// ---- State ----

const selected = ref(false)

// ---- Computed ----

const childCount = computed(() => {
  if (!props.node.children) return '—'
  const folders = props.node.children.filter(c => c.type === 'folder').length
  const files = props.node.children.filter(c => c.type === 'file').length
  const parts: string[] = []
  if (folders > 0) parts.push(folders + ' folder' + (folders > 1 ? 's' : ''))
  if (files > 0) parts.push(files + ' file' + (files > 1 ? 's' : ''))
  return parts.join(', ') || '0 items'
})

// ---- Methods ----

const onClick = () => {
  selected.value = !selected.value
  emit('select', props.node)
}

const onToggle = () => {
  emit('toggle', props.node)
}

const onChildSelect = (node: FileNode) => {
  emit('select', node)
}

const onChildToggle = (node: FileNode) => {
  emit('toggle', node)
}

</script>


<style scoped>
.fstn-wrapper { user-select: none; }
.fstn-row {
  display: flex; align-items: center; gap: 6px; padding: 6px 8px; border-radius: 6px;
  cursor: pointer; transition: background 0.15s;
}
.fstn-row:hover { background: #f1f5f9; }
.fstn-selected { background: #dbeafe; }
.fstn-selected:hover { background: #bfdbfe; }

.fstn-arrow { width: 16px; text-align: center; font-size: 0.65rem; color: #64748b; flex-shrink: 0; }
.fstn-arrow-placeholder { width: 16px; flex-shrink: 0; }

.fstn-icon { width: 24px; height: 24px; flex-shrink: 0; border-radius: 4px; overflow: hidden; }
.fstn-folder-icon { font-size: 1.1rem; width: 24px; text-align: center; flex-shrink: 0; }

.fstn-name { flex: 1; min-width: 0; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; font-size: 0.85rem; }

.fstn-size { font-size: 0.75rem; color: #94a3b8; flex-shrink: 0; min-width: 60px; text-align: right; }
.fstn-count { font-size: 0.7rem; color: #94a3b8; flex-shrink: 0; text-align: right; }

.fstn-children { }
</style>
