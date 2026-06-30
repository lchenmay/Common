
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
      if (rep?.Er == 'Unauthorized') {
        const rt = (globalThis as any).runtime
        if (rt) rt.session = ''
        try { localStorage.removeItem('session') } catch (_) {}
        alert('Session expired, please login again')
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