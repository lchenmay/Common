
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

export const loader = async (url: string, postdata: any, h: Function, ex: Function = () => {}) => {
  postdata.session = globalThis.runtime.session

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
      ex(rep)
    }
  } catch (err) {
    console.error('Loader error:', err)
    ex({ Er: 'NetworkError', message: err })
  }
}
