<template>
  <div class="markdown-container">
    <div v-if="loading" class="markdown-loading">
      <div class="loading-spinner"></div>
      <span>渲染中...</span>
    </div>
    <div v-else-if="renderedHtml" class="markdown-content" v-html="renderedHtml"></div>
    <div v-else class="markdown-empty">
      <span class="empty-icon">📝</span>
      <span>暂无内容</span>
    </div>
  </div>
</template>

<script setup lang="ts">
import { ref, watch, onMounted, nextTick } from 'vue'
import MarkdownIt from 'markdown-it'

// @ts-ignore
import markdownItKatex from 'markdown-it-katex'

// ✅ 只需要 KaTeX 的 CSS
import 'katex/dist/katex.min.css'
// ❌ 移除这行：markdown-it-katex 的样式通常不需要单独导入
// import 'markdown-it-katex/dist/katex.min.css'

const props = defineProps<{
  markdown: string
}>()

const renderedHtml = ref('')
const loading = ref(false)

// 配置 markdown-it
const md = new MarkdownIt({
  html: true,
  linkify: true,
  typographer: true,
  breaks: true,
  highlight: (str, lang) => {
    return `<pre><code class="language-${lang}">${str}</code></pre>`
  }
})

// 加载 KaTeX 插件
md.use(markdownItKatex, {
  throwOnError: false,
  errorColor: '#ef4444',
  macros: {
    "\\R": "\\mathbb{R}",
    "\\N": "\\mathbb{N}",
    "\\Z": "\\mathbb{Z}",
    "\\Q": "\\mathbb{Q}",
    "\\C": "\\mathbb{C}"
  }
})

const renderMarkdown = async () => {
  if (!props.markdown || typeof props.markdown !== 'string') {
    renderedHtml.value = ''
    return
  }

  loading.value = true

  try {
    const html = md.render(props.markdown)
    renderedHtml.value = html
    await nextTick()
  } catch (error) {
    console.error('Markdown 渲染失败:', error)
    renderedHtml.value = `
      <div class="render-error">
        <span>⚠️</span>
        <span>渲染失败: ${error instanceof Error ? error.message : String(error)}</span>
      </div>
    `
  } finally {
    loading.value = false
  }
}

watch(() => props.markdown, renderMarkdown, { immediate: true })
onMounted(renderMarkdown)

defineExpose({
  refresh: renderMarkdown
})
</script>

<style scoped>
/* ... 样式保持不变 ... */
.markdown-container {
  padding: 1.5rem;
  background: #fff;
  border-radius: 8px;
  min-height: 200px;
}

.markdown-loading {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  padding: 3rem;
  color: #94a3b8;
}

.loading-spinner {
  width: 32px;
  height: 32px;
  border: 3px solid #e2e8f0;
  border-top-color: #3b82f6;
  border-radius: 50%;
  animation: spin 0.8s linear infinite;
  margin-bottom: 1rem;
}

@keyframes spin {
  to { transform: rotate(360deg); }
}

.markdown-empty {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  padding: 3rem;
  color: #94a3b8;
  background: #f8fafc;
  border-radius: 8px;
}

.empty-icon {
  font-size: 3rem;
  margin-bottom: 0.5rem;
}

.render-error {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  padding: 1rem;
  color: #991b1b;
  background: #fee2e2;
  border-radius: 8px;
  border: 1px solid #fecaca;
}

.markdown-content {
  font-size: 16px;
  line-height: 1.8;
  color: #1e293b;
}

.markdown-content :deep(h1) {
  font-size: 2.25rem;
  font-weight: 700;
  margin-top: 2rem;
  margin-bottom: 1rem;
  padding-bottom: 0.5rem;
  border-bottom: 2px solid #e5e7eb;
}

.markdown-content :deep(h2) {
  font-size: 1.75rem;
  font-weight: 600;
  margin-top: 1.75rem;
  margin-bottom: 0.75rem;
  padding-bottom: 0.25rem;
  border-bottom: 1px solid #e5e7eb;
}

.markdown-content :deep(h3) {
  font-size: 1.4rem;
  font-weight: 600;
  margin-top: 1.5rem;
  margin-bottom: 0.5rem;
}

.markdown-content :deep(h4) {
  font-size: 1.15rem;
  font-weight: 600;
  margin-top: 1.25rem;
  margin-bottom: 0.5rem;
}

.markdown-content :deep(p) {
  margin-bottom: 1rem;
  line-height: 1.8;
}

.markdown-content :deep(a) {
  color: #3b82f6;
  text-decoration: none;
}

.markdown-content :deep(a:hover) {
  text-decoration: underline;
}

.markdown-content :deep(ul),
.markdown-content :deep(ol) {
  margin-bottom: 1rem;
  padding-left: 2rem;
}

.markdown-content :deep(li) {
  margin-bottom: 0.25rem;
  line-height: 1.8;
}

.markdown-content :deep(code) {
  background: #f1f5f9;
  padding: 0.15rem 0.4rem;
  border-radius: 4px;
  font-family: 'JetBrains Mono', 'Fira Code', monospace;
  font-size: 0.875em;
  color: #e11d48;
}

.markdown-content :deep(pre) {
  background: #0f172a;
  color: #e2e8f0;
  padding: 1.25rem;
  border-radius: 8px;
  overflow-x: auto;
  margin: 1rem 0;
  font-size: 0.875rem;
}

.markdown-content :deep(pre code) {
  background: transparent;
  color: inherit;
  padding: 0;
  font-size: 0.875rem;
}

.markdown-content :deep(blockquote) {
  border-left: 4px solid #3b82f6;
  padding: 0.5rem 1rem 0.5rem 1.5rem;
  margin: 1rem 0;
  background: #f8fafc;
  border-radius: 0 8px 8px 0;
  color: #475569;
  font-style: italic;
}

.markdown-content :deep(table) {
  width: 100%;
  border-collapse: collapse;
  margin: 1rem 0;
  font-size: 0.9rem;
}

.markdown-content :deep(th),
.markdown-content :deep(td) {
  border: 1px solid #e2e8f0;
  padding: 0.625rem 0.875rem;
  text-align: left;
}

.markdown-content :deep(th) {
  background: #f1f5f9;
  font-weight: 600;
}

.markdown-content :deep(tr:nth-child(even)) {
  background: #f8fafc;
}

.markdown-content :deep(hr) {
  border: none;
  border-top: 2px solid #e5e7eb;
  margin: 2rem 0;
}

.markdown-content :deep(img) {
  max-width: 100%;
  height: auto;
  border-radius: 8px;
  margin: 1rem 0;
}

/* KaTeX 公式样式 */
.markdown-content :deep(.katex) {
  font-size: 1.1em;
  font-family: 'KaTeX_Main', 'Times New Roman', serif;
}

.markdown-content :deep(.katex-display) {
  margin: 1.5rem 0;
  text-align: center;
  overflow-x: auto;
  overflow-y: hidden;
  padding: 0.75rem 0;
}

.markdown-content :deep(.katex-block-wrapper) {
  background: #f8fafc;
  border-radius: 8px;
  padding: 0.5rem 1.5rem;
  margin: 1rem 0;
  border: 1px solid #e2e8f0;
  overflow-x: auto;
}

.markdown-content :deep(.katex-inline) {
  vertical-align: middle;
}

.markdown-content :deep(.katex .msupsub) {
  font-size: 0.75em;
}
.markdown-content :deep(.katex .supsub) {
  font-size: 0.75em;
}

.markdown-content :deep(.katex .mfrac) {
  font-size: 0.85em;
}

.markdown-content :deep(.katex .sqrt) {
  font-size: 0.9em;
}

.markdown-content :deep(.katex .array) {
  font-size: 0.85em;
}

.markdown-content :deep(.katex .mathnormal) {
  font-family: 'KaTeX_Math', 'Times New Roman', serif;
  font-style: italic;
}

.markdown-content :deep(.katex .tag) {
  font-size: 0.85em;
  color: #64748b;
}

@media (max-width: 640px) {
  .markdown-container {
    padding: 0.75rem;
  }
  .markdown-content {
    font-size: 14px;
  }
  .markdown-content :deep(h1) {
    font-size: 1.5rem;
  }
  .markdown-content :deep(h2) {
    font-size: 1.25rem;
  }
  .markdown-content :deep(h3) {
    font-size: 1.1rem;
  }
  .markdown-content :deep(.katex-display) {
    font-size: 0.9em;
  }
}
</style>