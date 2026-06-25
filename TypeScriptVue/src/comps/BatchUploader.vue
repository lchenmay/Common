<template>
  <div class="batch-uploader">

    <!-- 拖拽上传区 -->
    <div class="bu-drop-zone" :class="{ 'bu-dragging': isDragging }"
      @dragover.prevent="isDragging = true" @dragleave.prevent="isDragging = false"
      @drop.prevent="onDrop" @click="triggerSelect">
      <input type="file" ref="fileInput" multiple style="display:none"
        :accept="props.accept" @change="onFileSelect" />
      <div class="bu-icon">📤</div>
      <div class="bu-text">{{ props.dropText || '拖拽文件到此处或点击选择' }}</div>
      <div class="bu-hint" v-if="props.acceptHint">{{ props.acceptHint }}</div>
      <div class="bu-hint" v-else-if="props.accept">支持: {{ props.accept }}</div>
    </div>

    <!-- 文件任务列表 -->
    <div v-if="tasks.length > 0" class="bu-task-list">
      <div class="bu-task-header">
        <span>文件列表 ({{ tasks.length }})</span>
        <span>{{ doneCount }}/{{ tasks.length }} 完成</span>
      </div>

      <div v-for="task in tasks" :key="task.id" class="bu-task-item">
        <div class="bu-task-icon">{{ fileIcon(task.file.name) }}</div>
        <div class="bu-task-info">
          <div class="bu-task-name" :title="task.file.name">{{ task.file.name }}</div>
          <div class="bu-task-size">{{ formatSize(task.file.size) }}</div>
          <!-- 进度条 -->
          <div class="bu-progress" v-if="task.status !== 'idle'">
            <div class="bu-progress-fill" :class="task.status"
              :style="{ width: task.progress + '%' }"></div>
          </div>
          <div class="bu-task-status" :class="task.status">
            {{ statusText(task) }}
            <span v-if="task.message" class="bu-task-msg"> - {{ task.message }}</span>
          </div>
        </div>
        <div class="bu-task-actions">
          <button v-if="task.status === 'error' || task.status === 'idle'"
            class="bu-btn-icon" title="重试" @click.stop="retryTask(task)">🔄</button>
          <button class="bu-btn-icon bu-btn-remove" title="移除" @click.stop="removeTask(task)">✕</button>
        </div>
      </div>
    </div>

    <!-- 操作按钮 -->
    <div v-if="tasks.length > 0 && !allDone" class="bu-actions">
      <button class="bu-btn bu-btn-upload" @click="uploadAll" :disabled="uploading">
        {{ uploading ? '上传中...' : '⬆️ 全部上传' }}
      </button>
      <button class="bu-btn bu-btn-clear" @click="clearDone">清除已完成</button>
    </div>

  </div>
</template>

<script setup lang="ts">
import { ref, computed } from 'vue'

// ========== 类型定义 ==========
export interface UploadTask {
  id: string
  file: File
  progress: number
  status: 'idle' | 'uploading' | 'success' | 'error'
  message?: string
  /** 服务端返回的文件记录 */
  rep?: any
}

export interface UploaderProps {
  /** 上传目标 URL */
  uploadUrl?: string
  /** 额外的 FormData 字段 */
  extraFields?: Record<string, string>
  /** 认证 token */
  authToken?: string
  /** 接受的文件类型，如 "image/*,.pdf" */
  accept?: string
  /** 接受类型的提示文字 */
  acceptHint?: string
  /** 拖拽区文字 */
  dropText?: string
  /** 最大文件大小 (bytes)，默认无限制 */
  maxSize?: number
  /** 是否自动开始上传 (选文件后立即上传，无需点按钮) */
  autoUpload?: boolean
}

const props = withDefaults(defineProps<UploaderProps>(), {
  uploadUrl: '/api/public/upload',
  autoUpload: false
})

// ========== 事件 ==========
const emit = defineEmits<{
  /** 单个文件上传成功 */
  (e: 'fileDone', task: UploadTask): void
  /** 单个文件上传失败 */
  (e: 'fileError', task: UploadTask): void
  /** 全部上传完成 */
  (e: 'allDone', tasks: UploadTask[]): void
}>()

// ========== 状态 ==========
const isDragging = ref(false)
const fileInput = ref<HTMLInputElement | null>(null)
const tasks = ref<UploadTask[]>([])
const uploading = ref(false)

const doneCount = computed(() => tasks.value.filter(t => t.status === 'success').length)
const allDone = computed(() => tasks.value.length > 0 && tasks.value.every(t => t.status === 'success' || t.status === 'error'))

// ========== 文件选择 ==========
const triggerSelect = () => fileInput.value?.click()

const onFileSelect = (e: Event) => {
  const files = (e.target as HTMLInputElement).files
  addFiles(files)
}

const onDrop = (e: DragEvent) => {
  isDragging.value = false
  addFiles(e.dataTransfer?.files || null)
}

const addFiles = (fileList: FileList | null) => {
  if (!fileList) return
  Array.from(fileList).forEach(file => {
    if (props.maxSize && file.size > props.maxSize) {
      const task = makeTask(file)
      task.status = 'error'
      task.message = `超过大小限制 ${formatSize(props.maxSize)}`
      tasks.value.push(task)
      emit('fileError', task)
      return
    }
    tasks.value.push(makeTask(file))
  })
  if (props.autoUpload && tasks.value.some(t => t.status === 'idle')) {
    uploadAll()
  }
}

const makeTask = (file: File): UploadTask => ({
  id: Math.random().toString(36).slice(2, 10),
  file,
  progress: 0,
  status: 'idle'
})

// ========== 上传逻辑 ==========
const uploadAll = async () => {
  const idle = tasks.value.filter(t => t.status === 'idle' || t.status === 'error')
  if (idle.length === 0) return
  uploading.value = true

  // 并行上传（最多3个并发）
  const concurrency = 3
  for (let i = 0; i < idle.length; i += concurrency) {
    const batch = idle.slice(i, i + concurrency)
    await Promise.all(batch.map(t => executeUpload(t)))
  }

  uploading.value = false
  if (tasks.value.every(t => t.status !== 'idle' && t.status !== 'uploading')) {
    emit('allDone', [...tasks.value])
  }
}

const executeUpload = (task: UploadTask): Promise<void> => {
  task.status = 'uploading'
  task.progress = 0

  const formData = new FormData()
  formData.append('file', task.file)

  // 附加额外字段
  if (props.extraFields) {
    for (const [k, v] of Object.entries(props.extraFields)) {
      formData.append(k, v)
    }
  }

  return new Promise((resolve) => {
    const xhr = new XMLHttpRequest()

    xhr.upload.onprogress = (event) => {
      if (event.lengthComputable) {
        task.progress = Math.min(Math.round((event.loaded * 100) / event.total), 99)
      }
    }

    xhr.onload = () => {
      if (xhr.status >= 200 && xhr.status < 300) {
        try {
          const rep = JSON.parse(xhr.responseText)
          if (rep.Er === 'OK' || rep.Er === undefined) {
            task.status = 'success'
            task.progress = 100
            task.rep = rep
            emit('fileDone', task)
          } else {
            task.status = 'error'
            task.message = rep.Er || '未知错误'
            emit('fileError', task)
          }
        } catch {
          task.status = 'error'
          task.message = '响应解析失败'
          emit('fileError', task)
        }
      } else {
        task.status = 'error'
        task.message = `HTTP ${xhr.status}`
        emit('fileError', task)
      }
      resolve()
    }

    xhr.onerror = () => {
      task.status = 'error'
      task.message = '网络错误'
      emit('fileError', task)
      resolve()
    }

    xhr.open('POST', props.uploadUrl)

    if (props.authToken) {
      xhr.setRequestHeader('Authorization', `Bearer ${props.authToken}`)
    }

    xhr.send(formData)
  })
}

const retryTask = (task: UploadTask) => {
  task.status = 'idle'
  task.progress = 0
  task.message = undefined
  uploadAll()
}

const removeTask = (task: UploadTask) => {
  const idx = tasks.value.indexOf(task)
  if (idx >= 0) tasks.value.splice(idx, 1)
}

const clearDone = () => {
  tasks.value = tasks.value.filter(t => t.status !== 'success')
}

// ========== 显示辅助 ==========
const statusText = (task: UploadTask) => {
  switch (task.status) {
    case 'uploading': return `上传中 ${task.progress}%`
    case 'success': return '✓ 完成'
    case 'error': return '✗ 失败'
    default: return '等待上传'
  }
}

const formatSize = (bytes: number) => {
  if (bytes < 1024) return bytes + ' B'
  if (bytes < 1048576) return (bytes / 1024).toFixed(1) + ' KB'
  return (bytes / 1048576).toFixed(1) + ' MB'
}

const fileIcon = (name: string) => {
  const ext = name.split('.').pop()?.toLowerCase() || ''
  const map: Record<string, string> = {
    pdf: '📕', doc: '📘', docx: '📘', xls: '📗', xlsx: '📗',
    ppt: '📙', pptx: '📙', txt: '📄', md: '📝',
    jpg: '🖼️', jpeg: '🖼️', png: '🖼️', gif: '🖼️', svg: '🖼️', webp: '🖼️', bmp: '🖼️',
    mp4: '🎬', avi: '🎬', mov: '🎬', mkv: '🎬', webm: '🎬',
    mp3: '🎵', wav: '🎵', flac: '🎵', ogg: '🎵',
    zip: '📦', rar: '📦', '7z': '📦', tar: '📦', gz: '📦',
    js: '💛', ts: '💙', vue: '💚', py: '🐍', fs: '💜', cs: '💜',
    json: '📋', xml: '📋', html: '🌐', css: '🎨', sql: '🗄️',
    csv: '📊', yml: '⚙️', yaml: '⚙️', toml: '⚙️',
    exe: '⚡', dll: '🔧', iso: '💿',
    c: '⚙️', cpp: '⚙️', h: '⚙️', java: '☕', rs: '🦀', go: '🔵', rb: '💎', php: '🐘',
  }
  return map[ext] || '📄'
}
</script>

<style scoped>
/* 拖拽区 */
.bu-drop-zone {
  border: 2px dashed #334155;
  border-radius: 12px;
  padding: 2.5rem 1.5rem;
  text-align: center;
  background: #0f172a;
  cursor: pointer;
  transition: all 0.2s;
  margin-bottom: 16px;
}

.bu-drop-zone:hover {
  border-color: #3b82f6;
  background: #1e293b;
}

.bu-drop-zone.bu-dragging {
  border-color: #3b82f6;
  background: #1e3a5f;
}

.bu-icon {
  font-size: 2.5rem;
  margin-bottom: 8px;
}

.bu-text {
  color: #94a3b8;
  font-size: 14px;
}

.bu-hint {
  color: #475569;
  font-size: 12px;
  margin-top: 4px;
}

/* 任务列表 */
.bu-task-list {
  background: #1e293b;
  border: 1px solid #334155;
  border-radius: 12px;
  overflow: hidden;
  margin-bottom: 16px;
}

.bu-task-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 10px 16px;
  font-size: 13px;
  color: #64748b;
  border-bottom: 1px solid #334155;
}

/* 单个任务 */
.bu-task-item {
  display: flex;
  align-items: center;
  gap: 12px;
  padding: 12px 16px;
  border-bottom: 1px solid #1e293b;
  transition: background 0.15s;
}

.bu-task-item:last-child {
  border-bottom: none;
}

.bu-task-item:hover {
  background: #0f172a;
}

.bu-task-icon {
  font-size: 24px;
  flex-shrink: 0;
  width: 36px;
  text-align: center;
}

.bu-task-info {
  flex: 1;
  min-width: 0;
}

.bu-task-name {
  font-size: 13px;
  color: #e2e8f0;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
}

.bu-task-size {
  font-size: 11px;
  color: #64748b;
  margin-top: 2px;
}

/* 进度条 */
.bu-progress {
  height: 4px;
  background: #334155;
  border-radius: 2px;
  margin-top: 6px;
  overflow: hidden;
}

.bu-progress-fill {
  height: 100%;
  border-radius: 2px;
  transition: width 0.3s;
}

.bu-progress-fill.uploading {
  background: #3b82f6;
}

.bu-progress-fill.success {
  background: #22c55e;
}

.bu-progress-fill.error {
  background: #ef4444;
}

/* 状态文字 */
.bu-task-status {
  font-size: 11px;
  margin-top: 2px;
}

.bu-task-status.uploading {
  color: #3b82f6;
}

.bu-task-status.success {
  color: #22c55e;
}

.bu-task-status.error {
  color: #ef4444;
}

.bu-task-status.idle {
  color: #64748b;
}

.bu-task-msg {
  color: #94a3b8;
}

/* 操作按钮 */
.bu-task-actions {
  display: flex;
  gap: 4px;
  flex-shrink: 0;
}

.bu-btn-icon {
  width: 28px;
  height: 28px;
  display: flex;
  align-items: center;
  justify-content: center;
  border: 1px solid #334155;
  background: #1e293b;
  border-radius: 6px;
  cursor: pointer;
  font-size: 12px;
  color: #94a3b8;
  transition: all 0.15s;
}

.bu-btn-icon:hover {
  background: #334155;
  color: #e2e8f0;
}

.bu-btn-remove:hover {
  background: #dc2626;
  border-color: #dc2626;
  color: white;
}

/* 底部操作 */
.bu-actions {
  display: flex;
  gap: 8px;
  justify-content: flex-end;
}

.bu-btn {
  padding: 8px 20px;
  border: 1px solid #334155;
  background: #1e293b;
  color: #e2e8f0;
  border-radius: 8px;
  cursor: pointer;
  font-size: 14px;
  transition: all 0.2s;
}

.bu-btn:hover:not(:disabled) {
  background: #334155;
}

.bu-btn:disabled {
  opacity: 0.5;
  cursor: not-allowed;
}

.bu-btn-upload {
  background: #3b82f6;
  border-color: #3b82f6;
  color: white;
}

.bu-btn-upload:hover:not(:disabled) {
  background: #2563eb;
}

.bu-btn-clear {
  color: #94a3b8;
}
</style>
