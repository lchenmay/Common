import { createRouter, createWebHistory, createMemoryHistory, RouteRecordRaw } from 'vue-router'

/**
 * 创建项目路由。
 * routes 是项目特定的路由数组，通过参数传入。
 * memoryHistory: 是否使用 createMemoryHistory（WYI 用，Aiarwa 不用）
 */
export const createProjectRouter = (routes: RouteRecordRaw[], memoryHistory = false) => {
  const r = createRouter({
    history: memoryHistory ? createMemoryHistory() : createWebHistory(),
    routes
  })

  return {
    router: r,

    navigate: (href:string,name:string,id:number) => {
      let url = href
      if(url.indexOf('/zh/') > 0){
        (globalThis as any).runtime.lang = 'zh'
        localStorage.setItem("runtime.lang", (globalThis as any).runtime.lang)
        url = url.replace('/zh/','/')
      }
      if(url.indexOf('/en/') > 0){
        (globalThis as any).runtime.lang = 'en'
        localStorage.setItem("runtime.lang", (globalThis as any).runtime.lang)
        url = url.replace('/en/','/')
      }
      window.location.href = url
      if(id != 0)
        r.push({ name: name, params: { id: id } })
      else
        r.push(name)
    },

    incomingRoute: () => {
      let path = window.location.pathname
      let hit = false
      const pages = [] as string[]
      pages.forEach((page:string) => {
        let pattern = "/" + page + "/"
        if(path.startsWith(pattern)){
          hit = true
          let id = path.substring(pattern.length)
          r.push({ name: page, params: { id: id }})
        }
      })
      if(hit == false){
        if(path.startsWith("/admin"))
          r.push('/admin')
        else
          r.push('/')
      }
    }
  }
}
