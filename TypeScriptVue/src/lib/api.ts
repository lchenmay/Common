
export const checkUrl = (url: string) => {
  // 内部函数用于获取基础地址
  const getbase = () => {
    // 1. 如果运行时明确指定了 api 地址（如生产环境配置），则使用它
    if (runtime.host.api) {
      return runtime.host.api
    }
    
    // 2. 关键点：如果是开发环境 (bun dev)，返回空字符串使用相对路径
    // 这样请求 /api/... 就会被 vite.config.ts 里的 proxy 转发到 https://localhost
    if (import.meta.env.DEV) {
      return "" 
    }

    // 3. 默认回退（通常用于生产环境或未配置情况）
    return `https://localhost`
  }

  // 如果 url 已经是 http/https 开头的绝对路径，直接返回
  if (/^http(s?):/i.test(url)) {
    return url
  }

  // 拼接基础地址和路径
  const base = getbase()
  
  // 确保 base 和 url 之间的斜杠处理正确
  if (base.endsWith('/') && url.startsWith('/')) {
    return base + url.slice(1)
  } else if (!base.endsWith('/') && !url.startsWith('/') && base !== "") {
    return base + '/' + url
  }
  
  return base + url
}

const request = (method: "POST" | "GET") => async (url: string, data: Record<string, any>) => {

  url = checkUrl(url)

  // --- 关键修改：截获 Clerk Token ---
  /*let clerkToken = null
  if (window.Clerk?.session) {
      // 异步获取 Clerk 签发的 JWT 令牌
      clerkToken = await window.Clerk.session.getToken()
  }*/

  const inits: RequestInit = {
    method: method,
    mode: 'cors',
    headers: { 
      'Content-Type': 'application/json',
      // 将 Token 注入 Header 
      //'Authorization': clerkToken ? `Bearer ${clerkToken}` : ''
     }
  }
  switch (method) {
    case "POST":
      if (runtime.session) { data['session'] = runtime.session }
      inits['body'] = JSON.stringify(data)
      break
    case "GET":
      const queryString = new URLSearchParams(data).toString()
      url = queryString ? `${url}?${queryString}` : url;
      break
  }
  return fetch(url, inits).then(res => { return res.json() }).catch(err => { console.error('Error:', err) })
}

export const upload = 
  (suc:Function,fail:Function) => 
  (file:any,dst:string,desc:string) => {

  let formData = new FormData()
  formData.append("file", file)

  let url = checkUrl(dst)

  let reader = new FileReader()
  reader.onloadend = async() => {
    if(reader.error)
      fail(null)
    else{
      let rep = await fetch(url,{
        method: 'POST',
        headers:{
          'Content-Type': 'application/octet-stream',
          'Filename': encodeURIComponent(file.name),
          'Desc': encodeURIComponent(desc)
        },
        body: reader.result })

      if(rep.ok){
        if(suc){
          suc(rep)
        }
      }else{
        if(fail){
          fail(rep)
        }
      }
    }
  }

  reader.readAsArrayBuffer(file)
}

export const post = request("POST")
export const get = request("GET")

export const loader = async (url:string,post:any,h:Function,ex:Function = () => {}) => {
  post.session = runtime.session
  let rep = await post(url, post)
  if(rep.Er == "OK")
    h(rep)
  else
    ex(rep)
}
