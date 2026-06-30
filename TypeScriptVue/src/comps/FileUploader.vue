<template>

<div class="fu-container" :data-theme="theme">
  <!-- 拖拽上传区域 -->
  <div class="fu-dropzone" :class="{ 'fu-dragging': isDragging }"
    @dragover.prevent="isDragging = true"
    @dragleave.prevent="isDragging = false"
    @drop.prevent="onDrop"
    @click="triggerSelect">
    <input type="file" multiple ref="fileInput" @change="onFileSelect" hidden />
    <div class="fu-dropzone-icon">📄</div>
    <div class="fu-dropzone-text">{{ key__text('dropHint', props.lang) }}</div>
    <div class="fu-dropzone-sub">{{ key__text('dropHintSub', props.lang) }}</div>
  </div>

  <!-- 文件列表 -->
  <div v-if="files.length > 0" class="fu-list">
    <div v-for="(f, idx) in files" :key="f.uid" class="fu-item">
      <!-- 缩略图 -->
      <div class="fu-thumb">
        <Thumbnail
          v-if="f.file"
          :src="f.preview"
          :name="f.file.name"
          :fileId="f.fileId"
          :baseUrl="baseUrl"
        />
        <Thumbnail
          v-else-if="f.fileId != null"
          :fileId="f.fileId"
          :baseUrl="baseUrl"
        />
      </div>
      <!-- 文件信息 -->
      <div class="fu-info">
        <div class="fu-name" :title="f.file.name">{{ f.file.name }}</div>
        <div class="fu-size">{{ formatSize(f.file.size) }}</div>
        <!-- 进度条 -->
        <div class="fu-progress" v-if="f.status === 'uploading'">
          <div class="fu-progress-bar" :style="{ width: f.progress + '%' }"></div>
        </div>
        <!-- 状态 -->
        <div class="fu-status" :class="'fu-' + f.status">
          <span v-if="f.status === 'pending'">{{ key__text('pending', props.lang) }}</span>
          <span v-if="f.status === 'uploading'">{{ f.progress }}%</span>
          <span v-if="f.status === 'success'">{{ key__text('success', props.lang) }}</span>
          <span v-if="f.status === 'error'">{{ f.message || key__text('uploadError', props.lang) }}</span>
        </div>
      </div>
      <!-- 操作 -->
      <div class="fu-actions">
        <button v-if="f.status === 'pending'" class="fu-btn-upload" @click.stop="uploadOne(idx)">
          {{ key__text('upload', props.lang) }}
        </button>
        <button v-if="f.status === 'error'" class="fu-btn-retry" @click.stop="uploadOne(idx)">
          {{ key__text('retry', props.lang) }}
        </button>
        <button class="fu-btn-remove" @click.stop="remove(idx)">{{ key__text('remove', props.lang) }}</button>
      </div>
    </div>

    <!-- 底部批量操作 -->
    <div class="fu-footer" v-if="hasPending">
      <button class="fu-btn-upload-all" @click="uploadAll">{{ key__text('uploadAll', props.lang) }}</button>
    </div>
  </div>
</div>

</template>


<script setup lang="ts">

import { ref, reactive } from 'vue'
import Thumbnail from './Thumbnail.vue'
import { formatSize } from '../lib/util/misc'
import { theme } from '../lib/common'

// ---- Props ----

const props = defineProps<{
  lang?: string            // 'zh' | 'en'
  uploadUrl?: string       // 上传目标 URL，默认 '/api/public/upload'
  baseUrl?: string         // 后端基础 URL，用于拼接 /thumbnail/{id}
  accept?: string          // 接受的文件类型，如 'image/*,.pdf'
  multiple?: boolean       // 是否允许多选，默认 true
  autoUpload?: boolean     // 选择文件后自动上传，默认 false
  maxSize?: number         // 单文件最大字节数，默认 10GB
  headers?: Record<string, string>  // 额外的请求头
}>()

const emit = defineEmits<{
  (e: 'uploaded', file: FileItem): void
  (e: 'error', file: FileItem, err: string): void
}>()

// ---- Types ----

interface FileItem {
  uid: string
  file: File
  progress: number
  status: 'pending' | 'uploading' | 'success' | 'error'
  message?: string
  preview?: string          // 图片预览 data URL
  fileId?: number           // 上传成功后后端返回的文件 ID
  response?: any            // 上传成功后服务端返回的数据
}

// ---- 多语言 ----
import { key__text } from '../lib/util/lang'

// ---- State ----

// theme 从 common.ts 导入（响应式 ref）
const isDragging = ref(false)
const fileInput = ref<HTMLInputElement | null>(null)
const files = reactive<FileItem[]>([])

const hasPending = ref(false)

// ---- Helpers ----

let uidCounter = 0
const genUid = () => 'fu_' + (++uidCounter) + '_' + Date.now()

const updatePending = () => {
  hasPending.value = files.some(f => f.status === 'pending')
}

// ---- 文件选择 ----

const triggerSelect = () => fileInput.value?.click()

const onFileSelect = (e: Event) => {
  const fl = (e.target as HTMLInputElement).files
  handleFiles(fl)
  if (fileInput.value) fileInput.value.value = ''
}

const onDrop = (e: DragEvent) => {
  isDragging.value = false
  handleFiles(e.dataTransfer?.files || null)
}

const handleFiles = (fileList: FileList | null) => {
  if (!fileList) return
  const max = props.maxSize || 10 * 1024 * 1024 * 1024 // 10GB
  Array.from(fileList).forEach(file => {
    if (file.size > max) {
      files.push({
        uid: genUid(), file, progress: 0, status: 'error',
        message: 'File too large (max ' + formatSize(max) + ')'
      })
      return
    }
    const item: FileItem = { uid: genUid(), file, progress: 0, status: 'pending' }
    // 图片预览
    if (file.type.startsWith('image/')) {
      const reader = new FileReader()
      reader.onload = () => { item.preview = reader.result as string }
      reader.readAsDataURL(file)
    }
    files.push(item)
  })
  updatePending()
  // 自动上传
  if (props.autoUpload) uploadAll()
}

// ---- 上传逻辑 (XHR + 进度) ----

const uploadOne = (idx: number): Promise<void> => {
  return new Promise((resolve) => {
    const item = files[idx]
    if (!item || item.status === 'uploading' || item.status === 'success') { resolve(); return }

    item.status = 'uploading'
    item.progress = 0

    const formData = new FormData()
    formData.append('file', item.file)

    const xhr = new XMLHttpRequest()
    xhr.upload.onprogress = (e) => {
      if (e.lengthComputable) item.progress = Math.min(Math.round(e.loaded * 100 / e.total), 99)
    }
    xhr.onload = () => {
      if (xhr.status >= 200 && xhr.status < 300) {
        try {
          item.response = JSON.parse(xhr.responseText)
          // 尝试提取后端返回的文件 ID
          if (item.response?.file?.id) item.fileId = Number(item.response.file.id)
          else if (item.response?.file?.ID) item.fileId = Number(item.response.file.ID)
          item.status = 'success'
          item.progress = 100
          emit('uploaded', item)
        } catch {
          item.status = 'error'
          item.message = 'Invalid response'
          emit('error', item, 'Invalid response')
        }
      } else {
        item.status = 'error'
        item.message = 'HTTP ' + xhr.status
        emit('error', item, 'HTTP ' + xhr.status)
      }
      updatePending()
      resolve()
    }
    xhr.onerror = () => {
      item.status = 'error'
      item.message = 'Network error'
      emit('error', item, 'Network error')
      updatePending()
      resolve()
    }

    const url = props.uploadUrl || '/api/public/upload'
    xhr.open('POST', url)
    if (props.headers) {
      Object.entries(props.headers).forEach(([k, v]) => xhr.setRequestHeader(k, v))
    }
    xhr.send(formData)
  })
}

const uploadAll = async () => {
  for (let i = 0; i < files.length; i++) {
    if (files[i].status === 'pending' || files[i].status === 'error')
      await uploadOne(i)
  }
}

const remove = (idx: number) => {
  files.splice(idx, 1)
  updatePending()
}

// ---- 暴露方法 ----

defineExpose({
  files,
  uploadAll,
  remove,
})

</script>


<style scoped>
.fu-container { max-width: 800px; }
.fu-dropzone {
  border: 2px dashed #ccd0d7; border-radius: 8px; padding: 3rem 1rem;
  text-align: center; background: #f9fafc; cursor: pointer;
  transition: all 0.2s;
}
.fu-dropzone:hover { border-color: #94a3b8; background: #f1f5f9; }
.fu-dragging { border-color: #3b82f6; background: #eff6ff; }
.fu-dropzone-icon { font-size: 2.5rem; margin-bottom: 0.5rem; }
.fu-dropzone-text { font-size: 1rem; color: #334155; font-weight: 500; }
.fu-dropzone-sub { font-size: 0.8rem; color: #94a3b8; margin-top: 0.25rem; }

.fu-list { margin-top: 1rem; }
.fu-item {
  display: flex; align-items: center; gap: 0.75rem;
  padding: 0.75rem; background: white; border: 1px solid #e2e8f0;
  border-radius: 8px; margin-bottom: 0.5rem;
}
.fu-thumb { width: 48px; height: 48px; flex-shrink: 0; border-radius: 6px; overflow: hidden; background: #f1f5f9; display: flex; align-items: center; justify-content: center; }
.fu-thumb-img { width: 100%; height: 100%; object-fit: cover; }
.fu-thumb-icon { font-size: 1.5rem; }
.fu-info { flex: 1; min-width: 0; }
.fu-name { font-size: 0.85rem; color: #1e293b; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
.fu-size { font-size: 0.75rem; color: #94a3b8; margin-top: 2px; }
.fu-progress { height: 4px; background: #e2e8f0; border-radius: 2px; margin-top: 4px; overflow: hidden; }
.fu-progress-bar { height: 100%; background: #3b82f6; transition: width 0.3s; border-radius: 2px; }
.fu-status { font-size: 0.75rem; margin-top: 2px; }
.fu-success { color: #16a34a; }
.fu-error { color: #dc2626; }
.fu-uploading { color: #3b82f6; }
.fu-pending { color: #94a3b8; }
.fu-actions { display: flex; gap: 0.25rem; flex-shrink: 0; }
.fu-btn-upload, .fu-btn-retry, .fu-btn-upload-all {
  padding: 4px 10px; border: 1px solid #3b82f6; border-radius: 4px;
  background: #3b82f6; color: white; font-size: 0.75rem; cursor: pointer; transition: 0.15s;
}
.fu-btn-upload:hover, .fu-btn-retry:hover, .fu-btn-upload-all:hover { background: #2563eb; }
.fu-btn-remove {
  padding: 4px 8px; border: 1px solid #e2e8f0; border-radius: 4px;
  background: white; color: #94a3b8; font-size: 0.75rem; cursor: pointer; transition: 0.15s;
}
.fu-btn-remove:hover { color: #dc2626; border-color: #fecaca; background: #fef2f2; }
.fu-footer { margin-top: 0.5rem; text-align: right; }

/* ===== Dark Theme ===== */
[data-theme="dark"] .fu-dropzone {
  background: #1e293b;
  border-color: #475569;
}
[data-theme="dark"] .fu-dropzone:hover {
  border-color: #94a3b8;
  background: #334155;
}
[data-theme="dark"] .fu-dragging {
  border-color: #60a5fa;
  background: #1e3a5f;
}
[data-theme="dark"] .fu-dropzone-text {
  color: #e2e8f0;
}
[data-theme="dark"] .fu-dropzone-sub {
  color: #64748b;
}
[data-theme="dark"] .fu-item {
  background: #1e293b;
  border-color: #334155;
}
[data-theme="dark"] .fu-thumb {
  background: #0f172a;
}
[data-theme="dark"] .fu-name {
  color: #e2e8f0;
}
[data-theme="dark"] .fu-size {
  color: #64748b;
}
[data-theme="dark"] .fu-progress {
  background: #334155;
}
[data-theme="dark"] .fu-btn-remove {
  background: #1e293b;
  color: #64748b;
  border-color: #334155;
}
[data-theme="dark"] .fu-btn-remove:hover {
  color: #fca5a5;
  background: #450a0a;
  border-color: #7f1d1d;
}
[data-theme="dark"] .fu-pending {
  color: #64748b;
}
</style>
