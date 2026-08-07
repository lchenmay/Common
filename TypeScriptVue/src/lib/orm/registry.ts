// lib/orm/registry.ts
// 扫描 OrmMor 等 ORM 模块中的枚举常量，构建字段→枚举值索引
// 枚举常量命名规则：<entityPrefix><FieldName>Enum_<ValueName> = <number>

export interface EnumValue {
  value: number
  name: string       // 枚举值名，如 "Normal", "ExecutiveOffice"
}

export interface EnumField {
  entityPrefix: string  // 实体简写，如 "eu"
  fieldName: string     // 字段名，如 "AuthType"
  values: EnumValue[]   // 枚举值列表
}

export type CssFieldNameMap = Record<string, string>  // "eu.EmploymentType" -> "EmpType"

let _enumMap: Map<string, EnumField> = new Map()
let _cssFieldName: CssFieldNameMap = {}

/** 扫描 OrmMor 命名空间，构建枚举索引。可传入 cssFieldName 覆盖 CSS 类名中的字段名。 */
export function registerOrm(
  ns: Record<string, any>,
  opts?: { cssFieldName?: CssFieldNameMap }
): Map<string, EnumField> {
  const map = new Map<string, EnumField>()
  if (opts?.cssFieldName) _cssFieldName = { ...opts.cssFieldName }

  // 正则：entityPrefix(小写)+FieldName(首字母大写驼峰)+Enum_+ValueName
  const re = /^([a-z]+)([A-Z][a-zA-Z]*)Enum_(.+)$/

  for (const [k, v] of Object.entries(ns)) {
    if (typeof v !== 'number') continue
    const m = k.match(re)
    if (!m) continue
    const [, entityPrefix, fieldName, valueName] = m
    const key = `${entityPrefix}.${fieldName}`
    let entry = map.get(key)
    if (!entry) {
      entry = { entityPrefix, fieldName, values: [] }
      map.set(key, entry)
    }
    entry.values.push({ value: v, name: valueName })
  }

  _enumMap = map
  return map
}

/** 获取已注册的枚举映射 */
export function getEnumMap(): Map<string, EnumField> {
  return _enumMap
}

/** 获取覆盖后的 CSS 字段名 */
export function getCssFieldName(ep: string, fn: string): string {
  return _cssFieldName[`${ep}.${fn}`] ?? fn
}
