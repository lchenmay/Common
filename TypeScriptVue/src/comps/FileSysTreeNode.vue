<template>

<div class="fstn-mega" :class="'fstn-' + view" :data-theme="theme">

  <!-- ═══════════════════════════════
       GRID VIEW — Card
       ═══════════════════════════════ -->
  <div v-if="view === 'grid'" class="fstn-card" :class="{ 'fstn-selected': selected }" @click="onClick">
    <!-- Selection check -->
    <div class="fstn-sel-check" :class="{ show: selected || hover }" @mouseenter="hover = true" @mouseleave="hover = false">
      <svg v-if="selected" width="18" height="18" viewBox="0 0 24 24"><circle cx="12" cy="12" r="10" fill="#ff1a1a"/><polyline points="7 12 10.5 15.5 17 9" fill="none" stroke="#fff" stroke-width="2.5" stroke-linecap="round" stroke-linejoin="round"/></svg>
      <svg v-else width="18" height="18" viewBox="0 0 24 24"><circle cx="12" cy="12" r="10" fill="none" stroke="#bbb" stroke-width="1.5"/></svg>
    </div>

    <!-- ⋮ Menu button -->
    <button class="fstn-dots" :class="{ open: isMenuOpen }"
      @click.stop="toggleMenu" @mousedown.stop.prevent
      title="More actions">
      <svg width="18" height="18" viewBox="0 0 24 24" fill="currentColor"><circle cx="5" cy="12" r="2"/><circle cx="12" cy="12" r="2"/><circle cx="19" cy="12" r="2"/></svg>
    </button>

    <!-- Icon area -->
    <div class="fstn-icon-area">
      <Thumbnail
        v-if="node.type === 'file'"
        :name="node.name"
        :fileId="node.id"
        :baseUrl="baseUrl"
        class="fstn-thumb"
      />
      <div v-else class="fstn-folder-icon">
        <svg width="56" height="56" viewBox="0 0 24 24" fill="#ff1a1a" stroke="#ff1a1a" stroke-width="0.5">
          <path d="M2 6a2 2 0 0 1 2-2h5l2 3h9a2 2 0 0 1 2 2v10a2 2 0 0 1-2 2H4a2 2 0 0 1-2-2V6z"/>
        </svg>
      </div>
    </div>

    <!-- Name -->
    <div class="fstn-name" :title="node.name">{{ node.name }}</div>

    <!-- Meta -->
    <div class="fstn-meta">
      <template v-if="node.type === 'folder'">
        <span class="fstn-meta-text">{{ childCountText }}</span>
      </template>
      <template v-else>
        <span class="fstn-meta-text">{{ formatSize(node.size) }}</span>
        <span v-if="node.suffix" class="fstn-suffix">{{ node.suffix.toUpperCase() }}</span>
      </template>
    </div>

    <!-- ═══ Dropdown Menu ═══ -->
    <div v-if="isMenuOpen" class="fstn-menu">
      <template v-for="item in menuItems" :key="item.action">
        <div v-if="item.action === 'divider'" class="fstn-menu-divider"></div>
        <button v-else class="fstn-menu-item" :class="{ danger: item.danger }"
          @click.stop="doAction(item.action)">
          <span class="fstn-mi-icon" v-html="item.iconSvg"></span>
          <span class="fstn-mi-label">{{ item.label }}</span>
        </button>
      </template>
    </div>
  </div>


  <!-- ═══════════════════════════════
       LIST VIEW — Row
       ═══════════════════════════════ -->
  <div v-if="view === 'list'" class="fstn-row" :class="{ 'fstn-selected': selected }" @click="onClick">
    <div class="fstn-sel-check-list" :class="{ show: selected || hoverRow }"
      @mouseenter="hoverRow = true" @mouseleave="hoverRow = false">
      <svg v-if="selected" width="16" height="16" viewBox="0 0 24 24"><circle cx="12" cy="12" r="10" fill="#ff1a1a"/><polyline points="7 12 10.5 15.5 17 9" fill="none" stroke="#fff" stroke-width="2.5" stroke-linecap="round" stroke-linejoin="round"/></svg>
      <svg v-else width="16" height="16" viewBox="0 0 24 24"><circle cx="12" cy="12" r="10" fill="none" stroke="#bbb" stroke-width="1.5"/></svg>
    </div>

    <!-- Icon -->
    <Thumbnail
      v-if="node.type === 'file'"
      :name="node.name" :fileId="node.id" :baseUrl="baseUrl"
      class="fstn-thumb-list"
    />
    <div v-else class="fstn-folder-icon-list">
      <svg width="22" height="22" viewBox="0 0 24 24" fill="#ff1a1a" stroke="#ff1a1a" stroke-width="0.5">
        <path d="M2 6a2 2 0 0 1 2-2h5l2 3h9a2 2 0 0 1 2 2v10a2 2 0 0 1-2 2H4a2 2 0 0 1-2-2V6z"/>
      </svg>
    </div>

    <!-- Name -->
    <span class="fstn-name-list" :title="node.name">{{ node.name }}</span>

    <!-- Size -->
    <span class="fstn-size-list">{{ node.type === 'folder' ? '—' : formatSize(node.size) }}</span>

    <!-- Modified -->
    <span class="fstn-modified-list">{{ node.modified ? formatDate(node.modified) : '—' }}</span>

    <!-- ⋮ Menu -->
    <div class="fstn-actions-list">
      <button class="fstn-dots-list" :class="{ open: isMenuOpen }"
        @click.stop="toggleMenu" @mousedown.stop.prevent title="More actions">
        <svg width="16" height="16" viewBox="0 0 24 24" fill="currentColor"><circle cx="5" cy="12" r="2"/><circle cx="12" cy="12" r="2"/><circle cx="19" cy="12" r="2"/></svg>
      </button>
      <!-- Inline menu for list view -->
      <div v-if="isMenuOpen" class="fstn-menu fstn-menu-list">
        <template v-for="item in menuItems" :key="item.action">
          <div v-if="item.action === 'divider'" class="fstn-menu-divider"></div>
          <button v-else class="fstn-menu-item" :class="{ danger: item.danger }"
            @click.stop="doAction(item.action)">
            <span class="fstn-mi-icon" v-html="item.iconSvg"></span>
            <span class="fstn-mi-label">{{ item.label }}</span>
          </button>
        </template>
      </div>
    </div>
  </div>

</div>

</template>


<script setup lang="ts">

import { ref, computed } from 'vue'
import Thumbnail from './Thumbnail.vue'
import { formatSize } from '../lib/util/misc'
import type { FileNode, MenuAction } from './FileSysTree.vue'

// ---- Props ----

const props = defineProps<{
  theme?: string
  node: FileNode
  depth: number
  view: 'grid' | 'list'
  lang?: string
  baseUrl?: string
  openMenuId: number | null
}>()

const emit = defineEmits<{
  (e: 'select', node: FileNode): void
  (e: 'toggle', node: FileNode): void
  (e: 'menu-action', payload: MenuAction): void
  (e: 'menu-toggle', id: number | null): void
}>()

// ---- State ----

const theme = computed(() => props.theme || 'day')

const selected = ref(false)
const hover = ref(false)
const hoverRow = ref(false)

// ---- Menu state ----

const isMenuOpen = computed(() => props.openMenuId === props.node.id)

function toggleMenu() {
  if (isMenuOpen.value) {
    emit('menu-toggle', null)
  } else {
    emit('menu-toggle', props.node.id)
  }
}

function closeMenu() {
  emit('menu-toggle', null)
}

function doAction(action: string) {
  emit('menu-action', { action, node: props.node })
}

// ---- Menu items ----

interface MenuItem {
  action: string; label: string; iconSvg: string; danger?: boolean; disabled?: boolean; sepBefore?: boolean
}

const menuItems = computed<MenuItem[]>(() => {
  const isFile = props.node.type === 'file'
  const items: MenuItem[] = [
    { action: 'download', label: 'Download', iconSvg: '<svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><path d="M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4"/><polyline points="7 10 12 15 17 10"/><line x1="12" y1="15" x2="12" y2="3"/></svg>' },
    { action: 'rename', label: 'Rename', iconSvg: '<svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><path d="M17 3a2.83 2.83 0 1 1 4 4L7.5 20.5 2 22l1.5-5.5Z"/></svg>' },
  ]
  if (isFile) {
    items.push(
      { action: 'divider', label: '', iconSvg: '', sepBefore: true },
      { action: 'get-link', label: 'Get link', iconSvg: '<svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><path d="M10 13a5 5 0 0 0 7.54.54l3-3a5 5 0 0 0-7.07-7.07l-1.72 1.71"/><path d="M14 11a5 5 0 0 0-7.54-.54l-3 3a5 5 0 0 0 7.07 7.07l1.71-1.71"/></svg>' },
      { action: 'share', label: 'Share', iconSvg: '<svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><circle cx="18" cy="5" r="3"/><circle cx="6" cy="12" r="3"/><circle cx="18" cy="19" r="3"/><line x1="8.59" y1="13.51" x2="15.42" y2="17.49"/><line x1="15.41" y1="6.51" x2="8.59" y2="10.49"/></svg>' },
    )
  }
  items.push(
    { action: 'divider', label: '', iconSvg: '', sepBefore: true },
    { action: 'move', label: 'Move', iconSvg: '<svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><polyline points="5 9 2 12 5 15"/><polyline points="9 5 12 2 15 5"/><polyline points="15 19 12 22 9 19"/><polyline points="19 9 22 12 19 15"/><line x1="2" y1="12" x2="22" y2="12"/><line x1="12" y1="2" x2="12" y2="22"/></svg>' },
    { action: 'copy', label: 'Copy', iconSvg: '<svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><rect x="9" y="9" width="13" height="13" rx="2" ry="2"/><path d="M5 15H4a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2h9a2 2 0 0 1 2 2v1"/></svg>' },
    { action: 'divider', label: '', iconSvg: '', sepBefore: true },
    { action: 'delete', label: isFile ? 'Remove' : 'Remove folder', iconSvg: '<svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><polyline points="3 6 5 6 21 6"/><path d="M19 6v14a2 2 0 0 1-2 2H7a2 2 0 0 1-2-2V6m3 0V4a2 2 0 0 1 2-2h4a2 2 0 0 1 2 2v2"/></svg>', danger: true },
  )
  return items
})

// ---- Computed ----

const childCountText = computed(() => {
  if (!props.node.children) return '—'
  const folders = props.node.children.filter(c => c.type === 'folder').length
  const files = props.node.children.filter(c => c.type === 'file').length
  const parts: string[] = []
  if (folders > 0) parts.push(folders + ' folder' + (folders > 1 ? 's' : ''))
  if (files > 0) parts.push(files + ' file' + (files > 1 ? 's' : ''))
  return parts.join(', ') || 'Empty'
})

function formatDate(iso: string): string {
  if (!iso) return '—'
  const d = new Date(iso)
  const now = new Date()
  const diff = now.getTime() - d.getTime()
  const mins = Math.floor(diff / 60000)
  if (mins < 1) return 'Just now'
  if (mins < 60) return mins + 'm ago'
  const hours = Math.floor(mins / 60)
  if (hours < 24) return hours + 'h ago'
  const days = Math.floor(hours / 24)
  if (days < 7) return days + 'd ago'
  return d.toLocaleDateString('en-US', { month: 'short', day: 'numeric', year: d.getFullYear() !== now.getFullYear() ? 'numeric' : undefined })
}

// ---- Methods ----

const onClick = () => {
  closeMenu()
  selected.value = !selected.value
  emit('select', props.node)
}

</script>


<style scoped>

/* ═══════════════════════════════
   GRID CARD
   ═══════════════════════════════ */

.fstn-card {
  position: relative;
  display: flex; flex-direction: column; align-items: center;
  padding: 12px 8px 10px;
  background: #fff;
  border: 1.5px solid #e8e8ec;
  border-radius: 8px;
  cursor: pointer;
  transition: all 0.15s;
}
.fstn-card:hover {
  border-color: #d0d0d8;
  box-shadow: 0 2px 8px rgba(0,0,0,0.06);
}
.fstn-card.fstn-selected {
  border-color: #ff1a1a;
  background: #fff5f5;
  box-shadow: 0 0 0 1px rgba(255,26,26,0.15);
}

/* Selection check */
.fstn-sel-check {
  position: absolute; top: 6px; left: 6px;
  opacity: 0; transition: opacity 0.12s;
  line-height: 0; z-index: 2;
}
.fstn-sel-check.show { opacity: 1; }

/* ⋮ Dots button */
.fstn-dots {
  position: absolute; top: 4px; right: 2px;
  display: flex; align-items: center; justify-content: center;
  width: 28px; height: 28px;
  border: none; background: transparent; color: #bbb;
  border-radius: 6px; cursor: pointer;
  opacity: 0; transition: all 0.12s;
  z-index: 2;
}
.fstn-card:hover .fstn-dots { opacity: 1; }
.fstn-dots:hover, .fstn-dots.open {
  background: #f0f0f4; color: #555; opacity: 1;
}

/* Icon area */
.fstn-icon-area {
  display: flex; align-items: center; justify-content: center;
  width: 64px; height: 64px; margin: 4px 0 8px;
}
.fstn-thumb {
  width: 60px; height: 60px; border-radius: 6px; overflow: hidden;
  object-fit: cover;
}
.fstn-folder-icon { line-height: 0; }

/* Name */
.fstn-name {
  width: 100%; text-align: center;
  font-size: 12px; font-weight: 500; color: #333;
  white-space: nowrap; overflow: hidden; text-overflow: ellipsis;
  padding: 0 4px; margin-bottom: 4px;
  line-height: 1.3;
}

/* Meta */
.fstn-meta {
  display: flex; align-items: center; gap: 6px;
  font-size: 11px; color: #999;
}
.fstn-suffix {
  background: #f0f0f4; color: #888;
  padding: 1px 5px; border-radius: 3px;
  font-size: 9px; font-weight: 700; letter-spacing: 0.05em;
}

/* ═══════════════════════════════
   LIST ROW
   ═══════════════════════════════ */

.fstn-row {
  display: flex; align-items: center; gap: 10px;
  padding: 8px 16px;
  border-bottom: 1px solid #f0f0f4;
  cursor: pointer; transition: background 0.1s;
  position: relative;
}
.fstn-row:hover { background: #fafafc; }
.fstn-row.fstn-selected { background: #fff5f5; }

.fstn-sel-check-list {
  width: 20px; flex-shrink: 0; line-height: 0;
  opacity: 0; transition: opacity 0.12s;
}
.fstn-sel-check-list.show, .fstn-row:hover .fstn-sel-check-list { opacity: 1; }

.fstn-thumb-list {
  width: 28px; height: 28px; border-radius: 4px; overflow: hidden;
  flex-shrink: 0; object-fit: cover;
}
.fstn-folder-icon-list { width: 28px; flex-shrink: 0; line-height: 0; }

.fstn-name-list {
  flex: 1; min-width: 0;
  font-size: 13px; font-weight: 500; color: #333;
  white-space: nowrap; overflow: hidden; text-overflow: ellipsis;
}
.fstn-size-list {
  width: 90px; text-align: right; flex-shrink: 0;
  font-size: 12px; color: #999;
}
.fstn-modified-list {
  width: 110px; text-align: right; flex-shrink: 0;
  font-size: 12px; color: #999; padding-right: 8px;
}

.fstn-actions-list {
  width: 36px; flex-shrink: 0; display: flex; justify-content: flex-end;
  position: relative;
}
.fstn-dots-list {
  display: flex; align-items: center; justify-content: center;
  width: 28px; height: 28px;
  border: none; background: transparent; color: #bbb;
  border-radius: 6px; cursor: pointer;
  opacity: 0; transition: all 0.12s;
}
.fstn-row:hover .fstn-dots-list { opacity: 1; }
.fstn-dots-list:hover, .fstn-dots-list.open {
  background: #f0f0f4; color: #555; opacity: 1;
}

/* ═══════════════════════════════
   DROPDOWN MENU (shared)
   ═══════════════════════════════ */

.fstn-menu {
  position: absolute;
  top: 32px; right: 0;
  min-width: 180px;
  background: #fff;
  border: 1px solid #e8e8ec;
  border-radius: 8px;
  box-shadow: 0 4px 20px rgba(0,0,0,0.12), 0 0 0 1px rgba(0,0,0,0.04);
  padding: 4px 0;
  z-index: 1000;
  overflow: hidden;
}
.fstn-menu-list { top: 34px; }

.fstn-menu-item {
  display: flex; align-items: center; gap: 10px;
  width: 100%; padding: 8px 14px;
  border: none; background: none;
  font-size: 13px; color: #444; cursor: pointer;
  transition: background 0.1s;
  text-align: left;
}
.fstn-menu-item:hover { background: #f5f5f7; }

.fstn-menu-item.danger { color: #e60000; }
.fstn-menu-item.danger:hover { background: #fff0f0; }

.fstn-menu-divider {
  height: 1px; margin: 4px 8px; background: #eee;
}

.fstn-mi-icon { width: 16px; display: flex; align-items: center; justify-content: center; flex-shrink: 0; opacity: 0.7; color: currentColor; }
.fstn-mi-icon :deep(svg) { width: 14px; height: 14px; }
.fstn-mi-label { white-space: nowrap; }


/* ===== Dark Theme ===== */
[data-theme="dark"] .fstn-card {
  background: #1e293b;
  border-color: #334155;
}
[data-theme="dark"] .fstn-card:hover {
  border-color: #475569;
  box-shadow: 0 2px 8px rgba(0,0,0,0.3);
}
[data-theme="dark"] .fstn-card.fstn-selected {
  border-color: #ff4444;
  background: #2d1518;
  box-shadow: 0 0 0 1px rgba(255,68,68,0.2);
}
[data-theme="dark"] .fstn-dots {
  color: #64748b;
}
[data-theme="dark"] .fstn-dots:hover,
[data-theme="dark"] .fstn-dots.open {
  background: #334155;
  color: #94a3b8;
}
[data-theme="dark"] .fstn-name {
  color: #e2e8f0;
}
[data-theme="dark"] .fstn-meta {
  color: #64748b;
}
[data-theme="dark"] .fstn-suffix {
  background: #334155;
  color: #94a3b8;
}
[data-theme="dark"] .fstn-row {
  border-bottom-color: #1e293b;
}
[data-theme="dark"] .fstn-row:hover {
  background: #1e293b;
}
[data-theme="dark"] .fstn-row.fstn-selected {
  background: #2d1518;
}
[data-theme="dark"] .fstn-name-list {
  color: #e2e8f0;
}
[data-theme="dark"] .fstn-size-list,
[data-theme="dark"] .fstn-modified-list {
  color: #64748b;
}
[data-theme="dark"] .fstn-dots-list {
  color: #64748b;
}
[data-theme="dark"] .fstn-dots-list:hover,
[data-theme="dark"] .fstn-dots-list.open {
  background: #334155;
  color: #94a3b8;
}
[data-theme="dark"] .fstn-menu {
  background: #1e293b;
  border-color: #334155;
  box-shadow: 0 4px 20px rgba(0,0,0,0.4), 0 0 0 1px rgba(0,0,0,0.2);
}
[data-theme="dark"] .fstn-menu-item {
  color: #e2e8f0;
}
[data-theme="dark"] .fstn-menu-item:hover {
  background: #334155;
}
[data-theme="dark"] .fstn-menu-item.danger {
  color: #f87171;
}
[data-theme="dark"] .fstn-menu-item.danger:hover {
  background: #450a0a;
}
[data-theme="dark"] .fstn-menu-divider {
  background: #334155;
}
</style>
