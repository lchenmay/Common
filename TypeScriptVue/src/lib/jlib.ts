import * as vue from 'vue'

/**
 * 项目级 jlib 聚合器工厂函数。
 * 由于 mor（CustomMor + OrmMor）是项目特定的代码生成产物，
 * 通过泛型参数 mor 传入。
 *
 * ws/notify/route/panel/host/runtime 等模块也需要项目提供，
 * 通过参数传入。
 */
export const createJlib = <T>(
  mor: T,
  deps: {
    ws: any,
    fetchs: { post: Function, get: Function },
    notify: any,
    route: any,
    panel: any,
    host: any,
    runtime: { setRT: Function, getRT: Function },
  }
) => ({
  vue,
  ws: deps.ws,
  notify: deps.notify,
  route: deps.route,
  panel: deps.panel,
  host: deps.host,
  runtime: (globalThis as any).runtime,
  mor: { studio: mor },
  post: deps.fetchs.post,
  get: deps.fetchs.get,
  setRT: deps.runtime.setRT,
  getRT: deps.runtime.getRT,
})
