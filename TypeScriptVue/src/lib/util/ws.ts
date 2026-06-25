const tryJSONobj = (data: any) => {
  try { return JSON.parse(data) }
  catch { return data }
}

export const createWebSocket_base = (wsbinHandler: Function, wsjsonHandler: Function) => (wsurl: string) => (_savetoRT: string = 'ws.conn'): WebSocket => {
  const ws = new WebSocket(wsurl);

  ws.onopen = () => { console.log('WebSocket connected') }
  ws.onclose = () => { console.log('WebSocket closed') }
  ws.onerror = (event) => { console.error('WebSocket error:', event) }
  ws.onmessage = (event) => {
    let msg = event.data
    switch (true) {
      case (typeof msg == 'string'):
        msg = tryJSONobj(msg)
        if (typeof msg !== 'string') wsjsonHandler(msg)
        break;
      default:
        wsbinHandler(msg)
        break;
    }
  }

  return ws;
}

export const disconnect = (ws: WebSocket) => {
  if (ws && ws.readyState === WebSocket.OPEN) ws.close()
}

export const trySend = (wsCtx: any) => (e: number) => (msg: Record<string, any> | Object) => {
  if (wsCtx && wsCtx.ws && wsCtx.ws.readyState === WebSocket.OPEN)
    wsCtx.ws.send(JSON.stringify({ e, val: msg }))
}

export const trySendx = (e: number) => (msg: Record<string, any> | Object) => {
  const wsCtx = (globalThis as any).runtime?.wsctx
  if (wsCtx) trySend(wsCtx)(e)(msg)
}
