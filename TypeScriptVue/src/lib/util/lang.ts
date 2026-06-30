import { getLang } from "../common"

const texts: Record<string, { zh: string; en: string }> = {
  // ---- 通用 ----
  'ER': { zh: '错误', en: 'Error' },

  // ---- TablePaged.vue 分页 ----
  'rpp': { zh: '记录/页', en: 'records/page' },
  'fst': { zh: '首页', en: 'First' },
  'prv': { zh: '上页', en: 'Prev' },
  'nxt': { zh: '下页', en: 'Next' },
  'lst': { zh: '末页', en: 'Last' },
  'total': { zh: '总页数', en: 'Total: ' },
  'current': { zh: '当前', en: 'Current: ' },

  // ---- FileUploader.vue ----
  'dropHint': { zh: '拖拽文件到此处，或点击选择', en: 'Drop files here or click to select' },
  'dropHintSub': { zh: '支持图片、PDF、文档等任意文件，单文件最大 10GB', en: 'Images, PDFs, documents supported, max 10GB per file' },
  'pending': { zh: '待上传', en: 'Pending' },
  'uploading': { zh: '上传中', en: 'Uploading' },
  'success': { zh: '上传成功', en: 'Success' },
  'uploadError': { zh: '上传失败', en: 'Failed' },
  'upload': { zh: '上传', en: 'Upload' },
  'retry': { zh: '重试', en: 'Retry' },
  'remove': { zh: '移除', en: 'Remove' },
  'uploadAll': { zh: '全部上传', en: 'Upload All' },

  // ---- FileSysTree.vue ----
  'loading': { zh: '加载中...', en: 'Loading...' },
  'empty': { zh: '暂无文件，拖放文件到此处上传', en: 'No files. Drop files here to upload.' },

  // ---- BatchUploader.vue ----
  'dropText': { zh: '拖拽文件到此处或点击选择', en: 'Drop files here or click to select' },
  'uploadingAll': { zh: '上传中...', en: 'Uploading...' },
  'uploadAllBtn': { zh: '⬆️ 全部上传', en: '⬆️ Upload All' },
  'unknownError': { zh: '未知错误', en: 'Unknown error' },
  'responseParseError': { zh: '响应解析失败', en: 'Response parse failed' },
  'networkError': { zh: '网络错误', en: 'Network error' },
  'statusDone': { zh: '✓ 完成', en: '✓ Done' },
  'statusError': { zh: '✗ 失败', en: '✗ Failed' },
  'statusIdle': { zh: '等待上传', en: 'Waiting' },
  'clearDone': { zh: '清除已完成', en: 'Clear Done' },
}

export const key__text = (key: string, lang?: string): string => {
  if (key in texts) {
    const i = texts[key]
    const currentLang = lang || getLang()
    if (currentLang === 'zh' || currentLang === 'en') {
      return i[currentLang]
    } else {
      return i['en']  //  fallback 到英文
    }
  } else {
    return key  // 修复：返回 key 而非空字符串
  }
}