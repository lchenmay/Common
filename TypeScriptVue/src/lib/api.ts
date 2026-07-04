
import { checkUrl } from './util/fetch'

export const loader = async (url: string, postdata: any, h: Function, ex: Function = () => {}) => {
  const session =
    (globalThis as any).runtime?.session ||
    localStorage.getItem('session') ||
    ''
  if (session) postdata.session = session

  let response: Response | undefined
  let fullUrl = ''

  try {
    fullUrl = checkUrl(url)
    response = await fetch(fullUrl, {
      method: 'POST',
      mode: 'cors',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify(postdata)
    })

    if (!response.ok) {
      throw new Error(`HTTP ${response.status}: ${response.statusText}`)
    }

    const rep = await response.json()

    if (rep?.Er == 'OK') {
      h(rep)
    } else {
      if (rep?.Er == 'Unauthorized' && !url.includes('/auth')) {
        // 判断是「会话过期」还是「权限不足」：
        // - /api/admin/* 要求 AuthType=Admin，非管理员也会拿到 Unauthorized（不是 session 过期）
        // - 只有当用户当前确实处于登录态（user.id>0）时，才把 401 视作 session 过期
        const rt = (globalThis as any).runtime
        const isAdminEndpoint = url.includes('/admin')
        const currentlyLoggedIn = !!(rt?.user?.eu?.id > 0)

        if (!isAdminEndpoint && currentlyLoggedIn) {
          if (rt) rt.session = ''
          try { localStorage.removeItem('session') } catch (_) {}
          alert('Session expired, please login again')
        }
        // 其余情况静默处理：
        // - 未登录访问 /api/admin/* → 交给业务组件决定如何提示
        // - 权限不足（非管理员访问 admin 端点）→ 业务组件应自行处理
      }
      ex(rep)
    }
  } catch (err) {
    console.error('Loader error:', err, '\nURL:', fullUrl)
    if (response) {
      console.error('Response status:', response.status)
      try { console.error('Response text:', await response.text()) } catch (_) {}
    }
    ex({ Er: 'NetworkError', message: (err as Error).message })
  }
}