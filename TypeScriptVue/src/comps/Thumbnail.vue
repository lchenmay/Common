<template>

<div class="tn-container">
  <!-- 图片：直接渲染 -->
  <img v-if="isImage && src" :src="src" class="tn-img" :class="fitClass" />

  <!-- 视频：原生播放器 -->
  <video v-else-if="isVideo && src" :src="src" controls class="tn-video" />

  <!-- 音频：原生播放器 -->
  <audio v-else-if="isAudio && src" :src="src" controls class="tn-audio" />

  <!-- PDF：后端缩略图 -->
  <img v-else-if="isPdf && thumbUrl" :src="thumbUrl" class="tn-img" />

  <!-- 其他文件类型：图标 -->
  <div v-else class="tn-icon">
    <span class="tn-icon-emoji">{{ fileIconDisplay }}</span>
    <div v-if="showName" class="tn-name">{{ name }}</div>
  </div>
</div>

</template>


<script setup lang="ts">

import { computed } from 'vue'
import { fileIcon } from '../lib/util/misc'

// ---- Props ----

const props = defineProps<{
  src?: string              // 文件 URL 或 data URL
  name?: string             // 文件名（用于图标和类型判断）
  fileId?: number           // 后端文件 ID（用于 /thumbnail/{id}）
  baseUrl?: string          // 后端基础 URL
  fit?: 'cover' | 'contain' // 图片缩放模式
  showName?: boolean        // 是否显示文件名
}>()

// ---- 类型判断 ----

const ext = computed(() => {
  const n = (props.name || '').toLowerCase()
  const dot = n.lastIndexOf('.')
  return dot >= 0 ? n.substring(dot + 1) : ''
})

const isImage = computed(() =>
  ['jpg','jpeg','png','gif','bmp','svg','webp','ico','heic','heif'].includes(ext.value)
)
const isVideo = computed(() =>
  ['mp4','webm','ogg','mov','avi','mkv'].includes(ext.value)
)
const isAudio = computed(() =>
  ['mp3','wav','ogg','flac','aac','m4a'].includes(ext.value)
)
const isPdf = computed(() => ext.value === 'pdf')

// ---- 缩略图 URL ----

const thumbUrl = computed(() => {
  if (props.src) return props.src
  if (props.fileId != null) {
    const base = props.baseUrl || ''
    return `${base}/thumbnail/${props.fileId}`
  }
  return ''
})

// ---- 文件图标 ----

const fileIconDisplay = computed(() => fileIcon(props.name || ''))

// ---- 缩放模式 ----

const fitClass = computed(() => {
  if (props.fit === 'contain') return 'tn-fit-contain'
  return 'tn-fit-cover'
})

</script>


<style scoped>
.tn-container { width: 100%; height: 100%; display: flex; align-items: center; justify-content: center; overflow: hidden; }
.tn-img { width: 100%; height: 100%; object-fit: cover; border-radius: 4px; }
.tn-fit-contain { object-fit: contain; }
.tn-video { width: 100%; height: 100%; border-radius: 4px; }
.tn-audio { width: 100%; margin: auto; }
.tn-icon { display: flex; flex-direction: column; align-items: center; justify-content: center; width: 100%; height: 100%; background: #f1f5f9; border-radius: 4px; }
.tn-icon-emoji { font-size: 2rem; }
.tn-name { font-size: 0.7rem; color: #64748b; margin-top: 4px; max-width: 100%; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; padding: 0 4px; }
</style>
