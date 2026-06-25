
export const checkUrl = (url: string) => {
  const getbase = () => {
    if ((globalThis as any).runtime?.host?.api) {
      return (globalThis as any).runtime.host.api
    }
    if (import.meta.env.DEV) {
      return "" 
    }
    return `https://localhost`
  }

  if (/^http(s?):/i.test(url)) {
    return url
  }

  const base = getbase()
  
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
      ex(rep)
    }
  } catch (err) {
    console.error('Loader error:', err)
    ex({ Er: 'NetworkError', message: err })
  }
}