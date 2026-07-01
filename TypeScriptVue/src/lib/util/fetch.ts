export const checkUrl = (url: string) => {
  const getbase = () => {
    const api = (globalThis as any).runtime?.host?.api
    if (api) return api
    if (import.meta.env.DEV || (typeof location !== 'undefined' && location.hostname === 'localhost'))
      return ""
    return ``
  }

  if (/^http(s?):/i.test(url)) return url

  const base = getbase()
  if (base.endsWith('/') && url.startsWith('/'))
    return base + url.slice(1)
  if (!base.endsWith('/') && !url.startsWith('/') && base !== "")
    return base + '/' + url
  return base + url
}

const request = (method: "POST" | "GET") => async (url: string, data: Record<string, any>) => {
  url = checkUrl(url)

  // Clerk auth token
  let clerkToken: string | null = null
  try {
    if ((window as any).Clerk?.session)
      clerkToken = await (window as any).Clerk.session.getToken()
  } catch {}

  const headers: Record<string, string> = { 'Content-Type': 'application/json' }
  if (clerkToken) headers['Authorization'] = `Bearer ${clerkToken}`

  const runtime = (globalThis as any).runtime
  if (method === "POST" && runtime?.session)
    data['session'] = runtime.session

  const fetchOptions: RequestInit = { method, headers }
  if (method === "POST") {
    fetchOptions.body = JSON.stringify(data)
  } else {
    const qs = new URLSearchParams(data as any).toString()
    if (qs) url = `${url}?${qs}`
  }

  // 超时控制 120s（Ollama 大模型首次推理可能较慢）
  const controller = new AbortController()
  const totalTimeoutId = setTimeout(() => controller.abort(), 120000)
  fetchOptions.signal = controller.signal

  try {
    const response = await fetch(url, fetchOptions)
    const text = await response.text()
    clearTimeout(totalTimeoutId)
    if (text.length === 0) return { Er: 'Empty response' }
    try {
      return JSON.parse(text)
    } catch {
      return { Er: 'JSON parse error' }
    }
  } catch (err: any) {
    clearTimeout(totalTimeoutId)
    if (err.name === 'AbortError') return { Er: 'Timeout' }
    console.error('[fetch]', err.message || err)
    return { Er: 'Fetch Error: ' + (err.message || String(err)) }
  }
}

export const upload =
  (suc: Function, fail: Function) =>
  (file: any, dst: string, desc: string) => {
    const url = checkUrl(dst)
    const reader = new FileReader()
    reader.onloadend = async () => {
      if (reader.error) { fail(null); return }
      try {
        const rep = await fetch(url, {
          method: 'POST',
          headers: {
            'Content-Type': 'application/octet-stream',
            'Filename': encodeURIComponent(file.name),
            'Desc': encodeURIComponent(desc)
          },
          body: reader.result
        })
        if (rep.ok) suc?.(rep)
        else fail?.(rep)
      } catch (e: any) {
        console.error('[upload]', e.message)
        fail?.(null)
      }
    }
    reader.onerror = () => fail(null)
    reader.readAsArrayBuffer(file)
  }

export const post = request("POST")
export const get = request("GET")
