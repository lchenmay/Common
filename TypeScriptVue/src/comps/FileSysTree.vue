<template>

<div class="fst-mega" :data-theme="theme">
  <!-- ═══ Toolbar ═══ -->
  <div class="fst-toolbar">
    <div class="fst-tb-left">
      <button class="fst-tb-btn fst-tb-primary" @click="emit('new-folder')">
        <svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><path d="M22 19a2 2 0 0 1-2 2H4a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h5l2 3h9a2 2 0 0 1 2 2z"/><line x1="12" y1="11" x2="12" y2="17"/><line x1="9" y1="14" x2="15" y2="14"/></svg>
        <span>New folder</span>
      </button>
    </div>
    <div class="fst-tb-right">
      <div class="fst-view-toggle">
        <button class="fst-vt-btn" :class="{ active: viewMode === 'grid' }" @click="viewMode = 'grid'" title="Grid view">
          <svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><rect x="3" y="3" width="7" height="7"/><rect x="14" y="3" width="7" height="7"/><rect x="3" y="14" width="7" height="7"/><rect x="14" y="14" width="7" height="7"/></svg>
        </button>
        <button class="fst-vt-btn" :class="{ active: viewMode === 'list' }" @click="viewMode = 'list'" title="List view">
          <svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><line x1="8" y1="6" x2="21" y2="6"/><line x1="8" y1="12" x2="21" y2="12"/><line x1="8" y1="18" x2="21" y2="18"/><line x1="3" y1="6" x2="3.01" y2="6"/><line x1="3" y1="12" x2="3.01" y2="12"/><line x1="3" y1="18" x2="3.01" y2="18"/></svg>
        </button>
      </div>
    </div>
  </div>

  <!-- ═══ Loading / Empty ═══ -->
  <div v-if="loading" class="fst-state">
    <div class="fst-spinner"></div>
    <span>{{ t('loading') }}</span>
  </div>
  <div v-else-if="nodes.length === 0" class="fst-state">
    <svg width="48" height="48" viewBox="0 0 24 24" fill="none" stroke="#ccc" stroke-width="1.5"><path d="M22 19a2 2 0 0 1-2 2H4a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h5l2 3h9a2 2 0 0 1 2 2z"/></svg>
    <span class="fst-empty-text">{{ t('empty') }}</span>
  </div>

  <!-- ═══ Grid View ═══ -->
  <div v-else-if="viewMode === 'grid'" class="fst-grid">
    <FileSysTreeNode
      v-for="node in nodes"
      :key="'g' + node.id"
      :node="node"
      :depth="0"
      :view="'grid'"
      :lang="props.lang"
      :baseUrl="props.baseUrl"
      :openMenuId="openMenuId"
      @select="onSelect"
      @toggle="onToggle"
      @menu-action="onMenuAction"
      @menu-toggle="onMenuToggle"
    />
  </div>

  <!-- ═══ List View ═══ -->
  <div v-else class="fst-list">
    <!-- header -->
    <div class="fst-list-header">
      <span class="fst-lh-name">Name</span>
      <span class="fst-lh-size">Size</span>
      <span class="fst-lh-modified">Modified</span>
      <span class="fst-lh-actions"></span>
    </div>
    <FileSysTreeNode
      v-for="node in nodes"
      :key="'l' + node.id"
      :node="node"
      :depth="0"
      :view="'list'"
      :lang="props.lang"
      :baseUrl="props.baseUrl"
      :openMenuId="openMenuId"
      @select="onSelect"
      @toggle="onToggle"
      @menu-action="onMenuAction"
      @menu-toggle="onMenuToggle"
    />
  </div>

  <!-- Backdrop for click-outside menu close -->
  <Teleport to="body">
    <div v-if="openMenuId !== null" class="fst-backdrop" @click="openMenuId = null"></div>
  </Teleport>
</div>

</template>


<script setup lang="ts">

import { ref, onMounted, computed } from 'vue'
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
  modified?: string  // ISO date string for "Modified" column
}

export interface MenuAction {
  action: string   // 'download' | 'rename' | 'move' | 'copy' | 'delete' | 'get-link' | 'share'
  node: FileNode
}

// ---- Props ----

const props = defineProps<{
  theme?: string
  lang?: string
  baseUrl?: string
  api?: string
  nodes?: FileNode[]
}>()

const emit = defineEmits<{
  (e: 'select', node: FileNode): void
  (e: 'rename-folder', node: FileNode): void
  (e: 'download-zip', node: FileNode): void
  (e: 'new-folder'): void
  (e: 'menu-action', payload: MenuAction): void
}>()

// ---- Lang ----

const t = (key: string): string => {
  const zh: Record<string, string> = {
    loading: '加载中...',
    empty: '暂无文件，拖放文件到此处上传',
  }
  const en: Record<string, string> = {
    loading: 'Loading...',
    empty: 'No files. Drop files here to upload.',
  }
  if (props.lang === 'zh') return zh[key] || key
  return en[key] || key
}

// ---- State ----

const theme = computed(() => props.theme || 'day')

const nodes = ref<FileNode[]>(props.nodes || [])
const loading = ref(false)
const viewMode = ref<'grid' | 'list'>('grid')
const openMenuId = ref<number | null>(null)

// ---- Methods ----

const onSelect = (node: FileNode) => {
  emit('select', node)
}

const onToggle = async (node: FileNode) => {
  node.expanded = !node.expanded
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

const onMenuAction = (payload: MenuAction) => {
  // Map new menu actions to legacy emits for backward compat
  if (payload.action === 'download') emit('download-zip', payload.node)
  else if (payload.action === 'rename') emit('rename-folder', payload.node)
  // New actions go through menu-action emit
  emit('menu-action', payload)
  openMenuId.value = null
}

const onMenuToggle = (id: number | null) => {
  openMenuId.value = id
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

/* ═══════════════════════════════
   Mega.nz style file system
   ═══════════════════════════════ */

/* --- Container --- */
.fst-mega {
  background: #f8f8fa;
  border-radius: 8px;
  overflow: hidden;
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif;
  font-size: 13px;
  color: #333;
}

/* --- Toolbar --- */
.fst-toolbar {
  display: flex; align-items: center; justify-content: space-between;
  padding: 10px 16px;
  background: #fff;
  border-bottom: 1px solid #e8e8ec;
}
.fst-tb-left { display: flex; gap: 8px; }
.fst-tb-right { display: flex; align-items: center; gap: 12px; }

.fst-tb-btn {
  display: inline-flex; align-items: center; gap: 6px;
  padding: 6px 14px; border-radius: 6px;
  font-size: 13px; font-weight: 500; cursor: pointer;
  border: 1px solid #e0e0e4; background: #fff; color: #555;
  transition: all 0.15s;
}
.fst-tb-btn:hover { background: #f5f5f7; border-color: #d0d0d4; }
.fst-tb-primary {
  background: #ff1a1a; color: #fff; border-color: #ff1a1a;
  font-weight: 600;
}
.fst-tb-primary:hover { background: #e60000; border-color: #e60000; }

/* View toggle */
.fst-view-toggle {
  display: flex; border: 1px solid #e0e0e4; border-radius: 6px; overflow: hidden;
}
.fst-vt-btn {
  display: flex; align-items: center; justify-content: center;
  width: 32px; height: 30px;
  border: none; background: #fff; color: #999; cursor: pointer;
  transition: all 0.15s;
}
.fst-vt-btn:hover { background: #f5f5f7; color: #555; }
.fst-vt-btn.active { background: #f0f0f4; color: #ff1a1a; }

/* --- State --- */
.fst-state {
  display: flex; flex-direction: column; align-items: center; justify-content: center;
  padding: 64px 16px; gap: 12px; color: #999;
}
.fst-empty-text { font-size: 14px; margin-top: 4px; }

.fst-spinner {
  width: 32px; height: 32px;
  border: 3px solid #e8e8ec; border-top-color: #ff1a1a;
  border-radius: 50%; animation: fst-spin 0.8s linear infinite;
}
@keyframes fst-spin { to { transform: rotate(360deg); } }

/* --- Grid --- */
.fst-grid {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(160px, 1fr));
  gap: 8px;
  padding: 12px;
}

/* --- List --- */
.fst-list { padding: 0; }

.fst-list-header {
  display: flex; align-items: center;
  padding: 8px 16px;
  font-size: 11px; font-weight: 600; text-transform: uppercase; letter-spacing: 0.05em;
  color: #999; border-bottom: 1px solid #e8e8ec;
}
.fst-lh-name { flex: 1; min-width: 0; }
.fst-lh-size { width: 90px; text-align: right; flex-shrink: 0; }
.fst-lh-modified { width: 110px; text-align: right; flex-shrink: 0; padding-right: 8px; }
.fst-lh-actions { width: 36px; flex-shrink: 0; }

/* --- Backdrop --- */
.fst-backdrop {
  position: fixed; top: 0; left: 0; right: 0; bottom: 0; z-index: 999;
}


/* ===== Dark Theme ===== */
[data-theme="dark"] .fst-mega {
  background: #0f172a;
  color: #e2e8f0;
}
[data-theme="dark"] .fst-toolbar {
  background: #1e293b;
  border-bottom-color: #334155;
}
[data-theme="dark"] .fst-tb-btn {
  background: #1e293b;
  color: #94a3b8;
  border-color: #334155;
}
[data-theme="dark"] .fst-tb-btn:hover {
  background: #334155;
  border-color: #475569;
  color: #e2e8f0;
}
[data-theme="dark"] .fst-vt-btn {
  background: #1e293b;
  color: #64748b;
}
[data-theme="dark"] .fst-vt-btn:hover {
  background: #334155;
  color: #94a3b8;
}
[data-theme="dark"] .fst-vt-btn.active {
  background: #1e3a5f;
  color: #ff4444;
}
[data-theme="dark"] .fst-view-toggle {
  border-color: #334155;
}
[data-theme="dark"] .fst-state {
  color: #64748b;
}
[data-theme="dark"] .fst-spinner {
  border-color: #334155;
}
[data-theme="dark"] .fst-list-header {
  color: #64748b;
  border-bottom-color: #334155;
}
</style>
