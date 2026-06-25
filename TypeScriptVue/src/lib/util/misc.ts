export const sleep = (ms: number) => {
  return new Promise(resolve => setTimeout(resolve, ms));
}

export const url__Params = (url: string): Record<string, string> => {
  const searchParams = new URLSearchParams(url.split('?')[1]);
  const params: Record<string, string> = {};
  
  for (const [key, value] of searchParams.entries()) {
    params[key] = value;
  }
  
  return params;
}

// ---- 文件工具函数 ----

/** 格式化文件大小为可读字符串 */
export const formatSize = (bytes: number): string => {
  if (bytes < 1024) return bytes + ' B'
  if (bytes < 1024 * 1024) return (bytes / 1024).toFixed(1) + ' KB'
  if (bytes < 1024 * 1024 * 1024) return (bytes / (1024 * 1024)).toFixed(1) + ' MB'
  return (bytes / (1024 * 1024 * 1024)).toFixed(2) + ' GB'
}

/** 根据文件名返回 emoji 图标 */
export const fileIcon = (name: string): string => {
  const ext = name.split('.').pop()?.toLowerCase() || ''
  if (['jpg','jpeg','png','gif','bmp','svg','webp','ico','heic','heif'].includes(ext)) return '🖼'
  if (ext === 'pdf') return '📕'
  if (['doc','docx'].includes(ext)) return '📘'
  if (['xls','xlsx','csv'].includes(ext)) return '📗'
  if (['ppt','pptx'].includes(ext)) return '📙'
  if (['zip','rar','7z','tar','gz'].includes(ext)) return '📦'
  if (['mp3','wav','ogg','flac','aac','m4a'].includes(ext)) return '🎵'
  if (['mp4','webm','ogg','mov','avi','mkv'].includes(ext)) return '🎬'
  if (['txt','md','json','xml','yml','yaml','log'].includes(ext)) return '📄'
  if (['html','htm'].includes(ext)) return '🌐'
  if (['js','ts','jsx','tsx','vue','svelte'].includes(ext)) return '💻'
  if (['py','rb','go','rs','java','c','cpp','cs','fs','swift','kt'].includes(ext)) return '⚙️'
  return '📎'
}

/** 判断文件是否为图片类型 */
export const isImage = (name: string): boolean => {
  const ext = name.split('.').pop()?.toLowerCase() || ''
  return ['jpg','jpeg','png','gif','bmp','svg','webp','ico','heic','heif'].includes(ext)
}