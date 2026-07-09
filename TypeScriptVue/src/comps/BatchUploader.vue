<template>
  <div class="batch-uploader">

    <!-- 拖拽上传区 -->
    <div class="bu-drop-zone" :class="{ 'bu-dragging': isDragging }"
      @dragover.prevent="isDragging = true" @dragleave.prevent="isDragging = false"
      @drop.prevent="onDrop" @click="triggerSelect">
      <input type="file" ref="fileInput" multiple style="display:none"
        :accept="props.accept" @change="onFileSelect" />
      <div class="bu-icon">{{ isDetecting ? '🔍' : '📤' }}</div>
      <div class="bu-text">
        <template v-if="isDetecting">正在检测文件夹结构...</template>
        <template v-else>{{ props.dropText || key__text('dropText', props.lang) }}</template>
      </div>
      <div class="bu-hint" v-if="isDetecting">
        已扫描 {{ detectingFileCount }} 个文件 / {{ detectingDirCount }} 个目录
      </div>
      <div class="bu-hint" v-else-if="props.enableFolderDrop && props.acceptHint">{{ props.acceptHint }} · 支持文件夹拖放</div>
      <div class="bu-hint" v-else-if="props.acceptHint">{{ props.acceptHint }}</div>
      <div class="bu-hint" v-else-if="props.accept">支持: {{ props.accept }}</div>
    </div>

    <!-- 检测覆盖层（阻断操作） -->
    <div v-if="isDetecting" class="bu-detection-overlay">
      <div class="bu-detection-box">
        <div class="mega-spinner"></div>
        <p class="bu-detection-title">正在检测文件夹结构…</p>
        <p class="bu-detection-progress">{{ detectingProgress }}</p>
        <p class="bu-detection-sub">已扫描 {{ detectingFileCount }} 个文件 / {{ detectingDirCount }} 个目录</p>
      </div>
    </div>

    <!-- 检测失败警告横幅 -->
    <div v-if="detectError" class="bu-detect-error">
      <div class="bu-detect-error-header">
        <span>⚠️ 文件夹结构超出限制，未加入上传列表</span>
        <button class="bu-detect-error-close" @click="detectError = null">✕</button>
      </div>
      <div class="bu-detect-error-body">
        <p>拖放内容未上传，原因：</p>
        <ul>
          <li v-if="detectError.depthExceeded">
            文件夹深度 {{ detectError.maxDepthFound }} 层，超出最大 {{ detectError.limitDepth }} 层限制
          </li>
          <li v-if="detectError.fileCountExceeded">
            文件总数 {{ detectError.fileCountFound }} 个，超出最大 {{ detectError.limitFiles }} 个限制
          </li>
        </ul>
        <p class="bu-detect-error-hint">
          建议：减少文件夹层级或分批拖放。
        </p>
      </div>
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
          <div class="bu-task-name" :title="displayPath(task)">
            <span v-if="task.relativePath" class="bu-task-folder-path">{{ task.relativePath }}</span>{{ task.file.name }}
          </div>
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
        {{ uploading ? key__text('uploadingAll', props.lang) : key__text('uploadAllBtn', props.lang) }}
      </button>
      <button class="bu-btn bu-btn-clear" @click="clearDone">{{ key__text('clearDone', props.lang) }}</button>
    </div>

  </div>
</template>

<script setup lang="ts">
import { ref, computed } from 'vue'
import { theme } from '../lib/common'
import { key__text } from '../lib/util/lang'

// 确保 theme 在 script 中被引用（TypeScript 不检查 template）
void theme.value

// ========== 类型定义 ==========
export interface UploadTask {
  id: string
  file: File
  progress: number
  status: 'idle' | 'uploading' | 'success' | 'error'
  message?: string
  /** 服务端返回的文件记录 */
  rep?: any
  /** 从拖放文件夹中解析出的相对路径，如 "subdir/nested/" */
  relativePath?: string
}

export interface DetectError {
  depthExceeded: boolean
  fileCountExceeded: boolean
  maxDepthFound: number
  fileCountFound: number
  limitDepth: number
  limitFiles: number
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
  /** 语言 */
  lang?: string
  /** 启用文件夹拖放（webkitGetAsEntry 递归遍历） */
  enableFolderDrop?: boolean
  /** 最大文件夹递归深度，默认 50 */
  maxFolderDepth?: number
  /** 最大文件夹内文件总数，默认 1000 */
  maxFolderFiles?: number
  /** 分片上传基础 URL（如 /api/public/uploadChunk），不传则不启用分片 */
  chunkUploadUrl?: string
  /** 文件 ≥ 此字节数触发分片上传 */
  chunkThresholdBytes?: number
  /** 每个分片的字节数，默认 512KB */
  chunkBytes?: number
}

const props = withDefaults(defineProps<UploaderProps>(), {
  uploadUrl: '/api/public/upload',
  autoUpload: false,
  enableFolderDrop: false,
  maxFolderDepth: 50,
  maxFolderFiles: 1000,
  chunkBytes: 512 * 1024
})

// ========== 事件 ==========
const emit = defineEmits<{
  /** 单个文件上传成功 */
  (e: 'fileDone', task: UploadTask): void
  /** 单个文件上传失败 */
  (e: 'fileError', task: UploadTask): void
  /** 全部上传完成 */
  (e: 'allDone', tasks: UploadTask[]): void
  /** 文件夹检测被拒绝 */
  (e: 'folderRejected', error: DetectError): void
}>()

// ========== 状态 ==========
// theme 从 common.ts 导入（响应式 ref）


const isDragging = ref(false)
const fileInput = ref<HTMLInputElement | null>(null)
const tasks = ref<UploadTask[]>([])
const uploading = ref(false)

// 文件夹检测状态
const isDetecting = ref(false)
const detectError = ref<DetectError | null>(null)
const detectingFileCount = ref(0)
const detectingDirCount = ref(0)
const detectingProgress = ref('')

const doneCount = computed(() => tasks.value.filter(t => t.status === 'success').length)
const allDone = computed(() => tasks.value.length > 0 && tasks.value.every(t => t.status === 'success' || t.status === 'error'))

// ========== 文件夹检测辅助 ==========
function readAllEntries(reader: FileSystemDirectoryReader): Promise<FileSystemEntry[]> {
  return new Promise((resolve) => {
    const all: FileSystemEntry[] = []
    const read = () => {
      reader.readEntries((entries) => {
        if (entries.length === 0) resolve(all)
        else { all.push(...entries); read() }
      })
    }
    read()
  })
}

async function detectFolderItems(items: DataTransferItemList): Promise<{
  ok: boolean
  tasks?: UploadTask[]
  error?: DetectError
}> {
  let fileCount = 0
  let maxDepth = 0
  const detectedTasks: UploadTask[] = []
  let aborted = false

  detectingFileCount.value = 0
  detectingDirCount.value = 0
  detectingProgress.value = ''

  async function walk(entry: FileSystemEntry, path: string, depth: number) {
    if (aborted) return
    if (depth > props.maxFolderDepth!) {
      if (depth > maxDepth) maxDepth = depth
      aborted = true; return
    }
    if (fileCount >= props.maxFolderFiles!) {
      aborted = true; return
    }

    if (entry.isFile) {
      fileCount++
      detectingFileCount.value = fileCount
      const file = await new Promise<File>((resolve, reject) => {
        (entry as FileSystemFileEntry).file(resolve, reject)
      })
      if (props.maxSize && file.size > props.maxSize) {
        aborted = true; return
      }
      const task = makeTask(file)
      task.relativePath = path
      detectedTasks.push(task)
    } else if (entry.isDirectory) {
      maxDepth = Math.max(maxDepth, depth)
      detectingDirCount.value++
      detectingProgress.value = `正在扫描：${path}${entry.name}/`
      const reader = (entry as FileSystemDirectoryEntry).createReader()
      const entries = await readAllEntries(reader)
      for (const child of entries) {
        await walk(child, path + entry.name + '/', depth + 1)
        if (aborted) return
      }
    }
  }

  for (const item of Array.from(items)) {
    const entry = item.webkitGetAsEntry?.()
    if (!entry) continue
    await walk(entry, '', 0)
    if (aborted) break
  }

  if (aborted) {
    return {
      ok: false,
      error: {
        depthExceeded: maxDepth > props.maxFolderDepth!,
        fileCountExceeded: fileCount > props.maxFolderFiles!,
        maxDepthFound: maxDepth,
        fileCountFound: fileCount,
        limitDepth: props.maxFolderDepth!,
        limitFiles: props.maxFolderFiles!
      }
    }
  }

  return { ok: true, tasks: detectedTasks }
}

// ========== 文件选择 ==========
const triggerSelect = () => fileInput.value?.click()

const onFileSelect = (e: Event) => {
  const files = (e.target as HTMLInputElement).files
  addFiles(files)
}

const onDrop = async (e: DragEvent) => {
  isDragging.value = false
  detectError.value = null

  if (props.enableFolderDrop && e.dataTransfer?.items) {
    const hasDirectory = Array.from(e.dataTransfer.items).some(
      item => item.webkitGetAsEntry?.()?.isDirectory
    )
    if (hasDirectory) {
      isDetecting.value = true
      const result = await detectFolderItems(e.dataTransfer.items)
      isDetecting.value = false
      if (!result.ok) {
        detectError.value = result.error!
        emit('folderRejected', result.error!)
        return
      }
      tasks.value.push(...result.tasks!)
      if (props.autoUpload) uploadAll()
      return
    }
  }

  // 纯文件 / 旧模式 / 无 webkitGetAsEntry → 原逻辑
  addFiles(e.dataTransfer?.files || null)
}

const addFiles = (fileList: FileList | null) => {
  if (!fileList) return
  Array.from(fileList).forEach(file => {
    // 如果文件有 webkitRelativePath（来自 input[webkitdirectory]），使用它
    const relPath = (file as any).webkitRelativePath
    if (props.maxSize && file.size > props.maxSize) {
      const task = makeTask(file)
      task.status = 'error'
      task.message = `超过大小限制 ${formatSize(props.maxSize)}`
      tasks.value.push(task)
      emit('fileError', task)
      return
    }
    const task = makeTask(file)
    if (relPath && typeof relPath === 'string') {
      // 剥离文件名，只保留目录路径，如 "dir/subdir/name.txt" → "dir/subdir/"
      const lastSlash = relPath.lastIndexOf('/')
      if (lastSlash > 0) task.relativePath = relPath.substring(0, lastSlash + 1)
    }
    tasks.value.push(task)
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

// ========== 上传分发 ==========
const executeUpload = (task: UploadTask): Promise<void> => {
  if (shouldUseChunked(task)) return executeChunkedUpload(task)
  return executeDirectUpload(task)
}

const shouldUseChunked = (task: UploadTask) =>
  !!(props.chunkUploadUrl && props.chunkThresholdBytes && task.file.size >= props.chunkThresholdBytes)

const executeDirectUpload = (task: UploadTask): Promise<void> => {
  task.status = 'uploading'
  task.progress = 0

  const formData = new FormData()
  formData.append('file', task.file)

  // 附加 relativePath
  if (task.relativePath) {
    formData.append('relativePath', task.relativePath)
  }

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
          // 兼容多文件返回数组格式
          if (Array.isArray(rep)) {
            const myRep = rep.find((r: any) => r?.file?.p?.Caption === task.file.name)
            if (myRep) {
              task.status = 'success'
              task.progress = 100
              task.rep = myRep
              emit('fileDone', task)
            } else {
              task.status = 'success'
              task.progress = 100
              task.rep = rep[0]
              emit('fileDone', task)
            }
          } else if (rep.Er === 'OK' || rep.Er === undefined) {
            task.status = 'success'
            task.progress = 100
            task.rep = rep
            emit('fileDone', task)
          } else {
            task.status = 'error'
            task.message = rep.Er || key__text('unknownError', props.lang)
            emit('fileError', task)
          }
        } catch {
          task.status = 'error'
          task.message = key__text('responseParseError', props.lang)
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
      task.message = key__text('networkError', props.lang)
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

const executeChunkedUpload = async (task: UploadTask): Promise<void> => {
  task.status = 'uploading'
  task.progress = 0
  task.message = undefined

  try {
    const chunkBytes = props.chunkBytes || 512 * 1024
    const totalChunks = Math.ceil(task.file.size / chunkBytes)
    const baseUrl = props.chunkUploadUrl!
    const session = props.extraFields?.session || ''

    // Stage 1: Init
    const initResp = await fetch(`${baseUrl}/init`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        session,
        fileName: task.file.name,
        fileSize: String(task.file.size),
        contentType: task.file.type || 'application/octet-stream',
        chunkSize: String(chunkBytes)
      })
    })
    const initData = await initResp.json()
    if (!initResp.ok || initData.Er !== 'OK') {
      task.status = 'error'
      task.message = initData.Er || `Init failed (HTTP ${initResp.status})`
      emit('fileError', task)
      return
    }
    const uploadId = initData.uploadId

    // Stage 2: Upload chunks
    for (let i = 0; i < totalChunks; i++) {
      const start = i * chunkBytes
      const end = Math.min(start + chunkBytes, task.file.size)
      const blob = task.file.slice(start, end)

      const form = new FormData()
      form.append('file', blob, `chunk_${i}`)
      form.append('uploadId', uploadId)
      form.append('session', session)
      form.append('chunkIndex', String(i))

      await new Promise<void>((resolve, reject) => {
        const xhr = new XMLHttpRequest()
        xhr.open('POST', baseUrl)
        xhr.onload = () => {
          if (xhr.status >= 200 && xhr.status < 300) resolve()
          else reject(new Error(`Chunk ${i}: HTTP ${xhr.status}`))
        }
        xhr.onerror = () => reject(new Error(`Chunk ${i}: network error`))
        xhr.send(form)
      })

      task.progress = Math.round(((i + 1) / totalChunks) * 95)
    }

    // Stage 3: Complete
    const folderId = props.extraFields?.folderId || '0'
    const completeResp = await fetch(`${baseUrl}/complete`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        session,
        uploadId,
        relativePath: task.relativePath || '',
        parentFolderId: folderId
      })
    })

    if (completeResp.ok) {
      const rep = await completeResp.json()
      task.status = 'success'
      task.progress = 100
      task.rep = rep
      emit('fileDone', task)
    } else {
      let errMsg = `Complete failed (HTTP ${completeResp.status})`
      try { const er = await completeResp.json(); errMsg = er.Er || errMsg } catch {}
      task.status = 'error'
      task.message = errMsg
      emit('fileError', task)
    }
  } catch (err: any) {
    task.status = 'error'
    task.message = err?.message || 'Chunked upload failed'
    emit('fileError', task)
  }
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
const displayPath = (task: UploadTask) => {
  return task.relativePath ? task.relativePath + task.file.name : task.file.name
}

const statusText = (task: UploadTask) => {
  switch (task.status) {
    case 'uploading': return key__text('uploadingAll', props.lang) + ` ${task.progress}%`
    case 'success': return key__text('statusDone', props.lang)
    case 'error': return key__text('statusError', props.lang)
    default: return key__text('statusIdle', props.lang)
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
  position: relative;
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

/* ===== 检测覆盖层 ===== */
.bu-detection-overlay {
  position: absolute;
  inset: 0;
  z-index: 10;
  display: flex;
  align-items: center;
  justify-content: center;
  background: rgba(15, 23, 42, 0.92);
  backdrop-filter: blur(4px);
  border-radius: 12px;
}

.bu-detection-box {
  text-align: center;
  padding: 32px;
}

.bu-detection-title {
  font-size: 16px;
  font-weight: 600;
  color: #e2e8f0;
  margin: 16px 0 8px 0;
}

.bu-detection-progress {
  font-size: 13px;
  color: #94a3b8;
  margin: 0 0 4px 0;
  word-break: break-all;
}

.bu-detection-sub {
  font-size: 12px;
  color: #64748b;
  margin: 0;
}

/* ===== 检测失败警告横幅 ===== */
.bu-detect-error {
  background: rgba(239, 68, 68, 0.1);
  border: 1px solid rgba(239, 68, 68, 0.3);
  border-radius: 10px;
  padding: 12px 16px;
  margin-bottom: 16px;
}

.bu-detect-error-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  font-size: 14px;
  font-weight: 600;
  color: #fca5a5;
  margin-bottom: 8px;
}

.bu-detect-error-close {
  background: none;
  border: none;
  color: #fca5a5;
  cursor: pointer;
  font-size: 14px;
  padding: 2px 6px;
  border-radius: 4px;
}

.bu-detect-error-close:hover {
  background: rgba(239, 68, 68, 0.2);
}

.bu-detect-error-body {
  font-size: 13px;
  color: #cbd5e1;
  line-height: 1.6;
}

.bu-detect-error-body p {
  margin: 0 0 6px 0;
}

.bu-detect-error-body ul {
  margin: 4px 0 8px 0;
  padding-left: 20px;
}

.bu-detect-error-body li {
  color: #fca5a5;
  font-size: 12px;
  margin-bottom: 2px;
}

.bu-detect-error-hint {
  font-size: 12px;
  color: #64748b;
  margin: 8px 0 0 0;
}

/* 文件夹路径显示 */
.bu-task-folder-path {
  color: #64748b;
  font-size: 11px;
  margin-right: 2px;
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

/* ===== Dark Theme ===== */
[data-theme="dark"] .bu-drop-zone {
  background: #1e293b;
  border-color: #475569;
}
[data-theme="dark"] .bu-drop-zone:hover {
  border-color: #60a5fa;
  background: #334155;
}
[data-theme="dark"] .bu-drop-zone.bu-dragging {
  border-color: #60a5fa;
  background: #1e3a5f;
}
[data-theme="dark"] .bu-text {
  color: #94a3b8;
}
[data-theme="dark"] .bu-task-list {
  background: #1e293b;
  border-color: #334155;
}
[data-theme="dark"] .bu-task-header {
  color: #64748b;
  border-bottom-color: #334155;
}
[data-theme="dark"] .bu-task-item {
  border-bottom-color: #1e293b;
}
[data-theme="dark"] .bu-task-item:hover {
  background: #0f172a;
}
[data-theme="dark"] .bu-task-name {
  color: #e2e8f0;
}
[data-theme="dark"] .bu-task-size {
  color: #64748b;
}
[data-theme="dark"] .bu-progress {
  background: #334155;
}
[data-theme="dark"] .bu-btn {
  background: #1e293b;
  border-color: #334155;
  color: #e2e8f0;
}
[data-theme="dark"] .bu-btn:hover:not(:disabled) {
  background: #334155;
}
[data-theme="dark"] .bu-btn-icon {
  background: #1e293b;
  border-color: #334155;
  color: #94a3b8;
}
[data-theme="dark"] .bu-btn-icon:hover {
  background: #334155;
  color: #e2e8f0;
}

/* ===== Spinner ===== */
.mega-spinner {
  width: 28px;
  height: 28px;
  border: 2px solid #334155;
  border-top-color: #3b82f6;
  border-radius: 50%;
  animation: bu-spin 0.7s linear infinite;
  margin: 0 auto;
}

@keyframes bu-spin {
  to { transform: rotate(360deg); }
}
</style>
