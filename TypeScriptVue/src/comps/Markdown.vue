<template>
  <div class="markdown-container">
    <div v-if="loading" class="markdown-loading">
      <span>加载中...</span>
    </div>
    <div v-else-if="renderedHtml" class="markdown-content" v-html="renderedHtml"></div>
    <div v-else class="markdown-empty">
      <span class="empty-icon">📝</span>
      <span>暂无内容</span>
    </div>
  </div>
</template>

<script setup lang="ts">
import { ref, watch, onMounted } from 'vue'
import { marked } from 'marked'
import katex from 'katex'
import 'katex/dist/katex.min.css'

const props = defineProps<{
  markdown: string
}>()

const renderedHtml = ref('')
const loading = ref(false)

// 渲染 LaTeX 公式
const renderLatex = (text: string): string => {
  if (typeof text !== 'string') return ''
  
  // 匹配行内公式：$...$ 或 \(...\)
  const inlineRegex = /\$([^\$]+?)\$|\\\((.+?)\\\)/g
  // 匹配块级公式：$$...$$ 或 \[...\]
  const blockRegex = /\$\$([^\$]+?)\$\$|\\\[(.+?)\\\]/gs
  
  let result = text
  
  // 先处理块级公式
  result = result.replace(blockRegex, (match, p1, p2) => {
    const latex = p1 || p2
    if (!latex) return match
    try {
      return katex.renderToString(latex, {
        displayMode: true,
        throwOnError: false,
        output: 'html'
      })
    } catch (e) {
      return `<div class="latex-error">LaTeX 渲染错误: ${latex}</div>`
    }
  })
  
  // 再处理行内公式
  result = result.replace(inlineRegex, (match, p1, p2) => {
    const latex = p1 || p2
    if (!latex) return match
    try {
      return katex.renderToString(latex, {
        displayMode: false,
        throwOnError: false,
        output: 'html'
      })
    } catch (e) {
      return `<span class="latex-error">LaTeX 渲染错误: ${latex}</span>`
    }
  })
  
  return result
}

// 渲染 Markdown
const renderMarkdown = async () => {
  // 确保 props.markdown 是字符串
  if (!props.markdown || typeof props.markdown !== 'string') {
    renderedHtml.value = ''
    return
  }
  
  loading.value = true
  try {
    // 使用异步方式解析 Markdown
    let html = await marked.parse(props.markdown)
    // 确保 html 是字符串
    if (typeof html !== 'string') {
      renderedHtml.value = ''
      return
    }
    // 再渲染 LaTeX 公式
    renderedHtml.value = renderLatex(html)
  } catch (error) {
    console.error('Markdown 解析失败:', error)
    renderedHtml.value = `<p style="color: red;">解析失败: ${error}</p>`
  } finally {
    loading.value = false
  }
}

// 监听 markdown 变化
watch(() => props.markdown, () => {
  renderMarkdown()
}, { immediate: true })

onMounted(() => {
  renderMarkdown()
})
</script>

<style scoped>
.markdown-container {
  padding: 1rem;
  background: #fff;
  border-radius: 8px;
}

.markdown-loading, .markdown-empty {
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

/* Markdown 内容样式 */
.markdown-content :deep(h1) {
  font-size: 2rem;
  font-weight: 600;
  margin-top: 1.5rem;
  margin-bottom: 1rem;
  padding-bottom: 0.5rem;
  border-bottom: 2px solid #e5e7eb;
}

.markdown-content :deep(h2) {
  font-size: 1.5rem;
  font-weight: 600;
  margin-top: 1.25rem;
  margin-bottom: 0.75rem;
  padding-bottom: 0.25rem;
  border-bottom: 1px solid #e5e7eb;
}

.markdown-content :deep(h3) {
  font-size: 1.25rem;
  font-weight: 600;
  margin-top: 1rem;
  margin-bottom: 0.5rem;
}

.markdown-content :deep(h4) {
  font-size: 1.1rem;
  font-weight: 600;
  margin-top: 0.75rem;
  margin-bottom: 0.5rem;
}

.markdown-content :deep(p) {
  margin-bottom: 1rem;
  line-height: 1.6;
  color: #334155;
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
  line-height: 1.6;
}

.markdown-content :deep(code) {
  background: #f1f5f9;
  padding: 0.2rem 0.4rem;
  border-radius: 4px;
  font-family: monospace;
  font-size: 0.875rem;
  color: #e11d48;
}

.markdown-content :deep(pre) {
  background: #1e293b;
  color: #e2e8f0;
  padding: 1rem;
  border-radius: 8px;
  overflow-x: auto;
  margin-bottom: 1rem;
}

.markdown-content :deep(pre code) {
  background: transparent;
  color: inherit;
  padding: 0;
}

.markdown-content :deep(blockquote) {
  border-left: 4px solid #3b82f6;
  padding-left: 1rem;
  margin-left: 0;
  margin-bottom: 1rem;
  color: #475569;
  font-style: italic;
}

.markdown-content :deep(table) {
  width: 100%;
  border-collapse: collapse;
  margin-bottom: 1rem;
}

.markdown-content :deep(th),
.markdown-content :deep(td) {
  border: 1px solid #e2e8f0;
  padding: 0.5rem 0.75rem;
  text-align: left;
}

.markdown-content :deep(th) {
  background: #f8fafc;
  font-weight: 600;
}

.markdown-content :deep(hr) {
  border: none;
  border-top: 1px solid #e5e7eb;
  margin: 1.5rem 0;
}

.markdown-content :deep(img) {
  max-width: 100%;
  height: auto;
  border-radius: 8px;
}

.markdown-content :deep(strong) {
  font-weight: 600;
  color: #0f172a;
}

.markdown-content :deep(em) {
  font-style: italic;
}

/* KaTeX 公式样式 */
.markdown-content :deep(.katex) {
  font-size: 1.1em;
}

.markdown-content :deep(.katex-display) {
  margin: 1rem 0;
  overflow-x: auto;
  overflow-y: hidden;
}

.markdown-content :deep(.katex-display > .katex) {
  display: inline-block;
  max-width: 100%;
}

.latex-error {
  color: #ef4444;
  background: #fee2e2;
  padding: 0.125rem 0.25rem;
  border-radius: 4px;
  font-size: 0.875rem;
}
</style>