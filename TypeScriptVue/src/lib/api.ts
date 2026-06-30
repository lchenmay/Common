
import { checkUrl } from './util/fetch'

export const loader = async (url: string, postdata: any, h: Function, ex: Function = () => {}) => {
  postdata.session = (globalThis as any).runtime?.session

  try {
    const fullUrl = checkUrl(url)
    const response = await fetch(fullUrl, {
      method: 'POST',
      mode: 'cors',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify(postdata)
    })
    
    const rep = await response.json()
    
    if (rep?.Er == "OK") {
      h(rep)
    } else {
      if (rep?.Er == "Unauthorized") {
        const rt = (globalThis as any).runtime
        if (rt) {
          rt.session = ""
          try { localStorage.removeItem("runtime.session") } catch (_) {}
          try { localStorage.removeItem("session") } catch (_) {}
        }
        alert('登录已过期，请重新登录')
      }
      ex(rep)
    }
  } catch (err) {
    console.error('Loader error:', err)
    ex({ Er: 'NetworkError', message: err })
  }
}