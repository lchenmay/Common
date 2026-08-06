// lib/orm/probe.ts
// 差分探针：通过 row__cell 纯函数反推列绑定了哪个 ORM 枚举/布尔字段

import type { TableField } from '../../comps/crud-types'
import type { EnumField, EnumValue } from './registry'
import { getEnumMap, getCssFieldName } from './registry'

export interface FieldBinding {
  /** 列的 key（表头） */
  fieldKey: string
  /** 实体简写，如 "eu" */
  entityPrefix: string
  /** 字段名，如 "AuthType" */
  fieldName: string
  /** CSS 类名前缀，如 "eu-AuthType" */
  cssClass: string
  /** 枚举值列表（含探针返回的 label） */
  values: (EnumValue & { label: string })[]
}

function deepClone<T>(obj: T): T {
  return JSON.parse(JSON.stringify(obj))
}

function setAtPath(obj: any, path: string, value: any) {
  const parts = path.split('.')
  let cur = obj
  for (let i = 0; i < parts.length - 1; i++) {
    cur = cur[parts[i]]
  }
  cur[parts[parts.length - 1]] = value
}

function getAtPath(obj: any, path: string): any {
  return path.split('.').reduce((o, k) => o?.[k], obj)
}

/** 在原型对象的 p 下找到所有可能是枚举/布尔字段的路径 */
function findLeafPaths(
  proto: any,
  prefix = '',
  enumMap: Map<string, EnumField>
): { path: string; entityPrefix: string; fieldName: string; kind: 'enum' | 'bool' }[] {
  const result: { path: string; entityPrefix: string; fieldName: string; kind: 'enum' | 'bool' }[] = []
  if (proto === null || typeof proto !== 'object') return result

  // 实体容器：{ eu: { p: {...}, id: 0 } }
  const entRe = /^[a-z]+$/   // 全小写 = 实体简写
  for (const key of Object.keys(proto)) {
    const val = proto[key]
    if (!entRe.test(key) || !val || typeof val !== 'object') continue
    if (!val.p || typeof val.p !== 'object') continue

    for (const pk of Object.keys(val.p)) {
      const fullPath = `${prefix}${key}.p.${pk}`
      const pv = val.p[pk]
      if (typeof pv === 'boolean') {
        result.push({ path: fullPath, entityPrefix: key, fieldName: pk, kind: 'bool' })
      } else if (typeof pv === 'number') {
        // 只有注册了枚举值的字段才算 enum
        if (enumMap.has(`${key}.${pk}`)) {
          result.push({ path: fullPath, entityPrefix: key, fieldName: pk, kind: 'enum' })
        }
      }
    }
  }

  return result
}

/**
 * 对 fields 中没有 row__style 的枚举/布尔列做差分探针，
 * 返回确认绑定的列（含 label 映射）。
 * 只检测 memoizedBinding 为空的列（首次一次探测）。
 */
const memoizedBinding = new WeakMap<object, FieldBinding[]>()

export function probeBindings(
  fields: TableField[],
  empty__data: () => any,
  enumMap?: Map<string, EnumField>
): FieldBinding[] {
  const cacheKey = fields
  const cached = memoizedBinding.get(cacheKey as any)
  if (cached) return cached

  const map = enumMap ?? getEnumMap()
  const proto = empty__data()
  const leafPaths = findLeafPaths(proto, '', map)

  const bindings: FieldBinding[] = []

  for (const f of fields) {
    if (!f.row__cell) continue  // 只跳过没有 row__cell 的列（纯展示列）

    let found = false

    // 先试 bool
    for (const lp of leafPaths.filter(l => l.kind === 'bool')) {
      const orig = getAtPath(proto, lp.path)
      const c1 = deepClone(proto); setAtPath(c1, lp.path, true)
      const c2 = deepClone(proto); setAtPath(c2, lp.path, false)
      try {
        const l1 = String(f.row__cell!(c1))
        const l2 = String(f.row__cell!(c2))
        if (l1 !== l2) {
          setAtPath(c1, lp.path, orig); setAtPath(c2, lp.path, orig) // restore
          const cssFieldName = getCssFieldName(lp.entityPrefix, lp.fieldName)
          bindings.push({
            fieldKey: f.key,
            entityPrefix: lp.entityPrefix,
            fieldName: lp.fieldName,
            cssClass: `${lp.entityPrefix}-${cssFieldName}`,
            values: [
              { value: 1, name: 'True', label: l1 },
              { value: 0, name: 'False', label: l2 }
            ]
          })
          found = true
          break
        }
        setAtPath(c1, lp.path, orig); setAtPath(c2, lp.path, orig)
      } catch { /* row__cell 可能访问了外部 state，跳过 */ }
    }
    if (found) continue

    // 再试 enum
    for (const lp of leafPaths.filter(l => l.kind === 'enum')) {
      const ef = map.get(`${lp.entityPrefix}.${lp.fieldName}`)
      if (!ef || ef.values.length < 2) continue

      const orig = getAtPath(proto, lp.path)
      const labels: string[] = []
      let matched = true
      for (const ev of ef.values) {
        const clone = deepClone(proto)
        setAtPath(clone, lp.path, ev.value)
        try {
          labels.push(String(f.row__cell!(clone)))
        } catch {
          matched = false
          break
        }
      }
      setAtPath(deepClone(proto), lp.path, orig) // restore

      if (matched && new Set(labels).size === labels.length) {
        const cssFieldName = getCssFieldName(lp.entityPrefix, lp.fieldName)
        bindings.push({
          fieldKey: f.key,
          entityPrefix: lp.entityPrefix,
          fieldName: lp.fieldName,
          cssClass: `${lp.entityPrefix}-${cssFieldName}`,
          values: ef.values.map((ev, i) => ({ ...ev, label: labels[i] }))
        })
        found = true
        break
      }
    }
  }

  memoizedBinding.set(cacheKey as any, bindings)
  return bindings
}

/** 生成自动 row__style 函数，返回 CSS 类名 */
export function makeAutoRowStyle(binding: FieldBinding): (row: any) => string {
  const ep = binding.entityPrefix
  const fn = binding.fieldName
  const cssFieldName = getCssFieldName(ep, fn)
  const byValue = new Map(binding.values.map(v => [v.value, v.name]))
  return (row: any): string => {
    const v = getAtPath(row, `${ep}.p.${fn}`)
    const name = byValue.get(v as number) ?? String(v)
    return `${ep}-${cssFieldName}-${name}`
  }
}

/** 生成 filterKey（后端参数名），通常 = fieldName 复数形式 */
export function makeFilterKey(binding: FieldBinding): string {
  const fn = binding.fieldName
  const overrides: Record<string, string> = {
    'AffiliateTier': 'affiliates',   // 后端 Db.fs 用 'affiliates'
  }
  if (overrides[fn]) return overrides[fn]
  return fn.toLowerCase() + 's'
}

/** 由 FieldBinding 生成读值函数（row -> 枚举/布尔值），供显式 filter 回退 get 使用 */
export function makeValueGetter(binding: FieldBinding): (row: any) => any {
  const path = `${binding.entityPrefix}.p.${binding.fieldName}`
  return (row: any) => getAtPath(row, path)
}
