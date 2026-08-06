import type { Component, Ref } from 'vue'

/**
 * 公共 CRUD 组件的类型定义。
 * 抽离到独立文件，避免在 <script setup> 内 export interface 触发
 * TS1184，同时让 d.ts 能正确引用（消除 TS4082）。
 */

export interface FieldFilterOption {
  /** 选中态样式：字符串 = 引用样式表的 class 名；对象 = inline 样式 */
  style?: string | Record<string, string>
}

export interface FieldFilter {
  /** 选项数组；下标即 value（= 字段枚举值）。文本 label 由同字段的 row__cell 自动派生 */
  options: FieldFilterOption[]
  /** 读取值函数；不传则回退 probe 探测路径（或 row[key]） */
  get?: (row: any) => any
  /** 后端接收的数组参数名；不传则按字段推导 */
  paramName?: string
  /** 是否把命中选项的 style 应用到行单元格，默认 true */
  applyRowStyle?: boolean
  /** 派生 label 时用于构造合成行的取值路径（如 'eu.p.AuthType'）；
   *  缺省则 label 退化为 String(下标) */
  valuePath?: string
}

export interface TableField {
  key: string
  row__cell?: Function
  row__style?: Function
  /** inline 行单元格样式（与 row__style 的 class 名互补；对象形式优先） */
  row__style_inline?: Function
  /** 字段级筛选配置：在 Crud 上生成复选过滤器，并把样式注入筛选条与行单元格 */
  filter?: FieldFilter
  sortable?: boolean
  style?: string | Function
  text?: string
  width?: string
}

export interface Paging {
  npp: number
  page: number
  total: number
  pages: number
}

/**
 * 列表聚合显示配置。
 * 后端在 `ls` 响应的 `aggregates` 字段里返回 { 字段名: 数值 }，
 * 这里按 field 把值落到同名列（fields[].key）的表尾单元格。
 * 约定：只有数值字段（float / integer）可以聚合；非数值一律显示 '-'。
 */
export interface AggregateConfig {
  /** 必须同时是列的 key 与后端 aggregates 的键 */
  field: string
  /** 可选前缀标题（如 "Total"），只在第一个有值的聚合列上显示 */
  label?: string
  /** 可选格式化，入参恒为 number */
  format?: (v: number) => string
}

export interface TablePagedProps<Data = any> {
  lang?: string
  fields: TableField[]
  api: string
  hpostdata?: Function
  onRowClick?: (data: Data) => void
  selected?: Data[]
  defaultSort?: string
  /** 声明哪些数值列在表尾显示聚合值；不传则不渲染表尾 */
  aggregate?: AggregateConfig[]
  /** 外部触发重载（Crud 传入 filterTrigger ref） */
  trigger?: Ref<number> | number
}

export interface CrudProps<Data = any> {
  lang?: string
  caption?: string
  api?: string
  fields?: TableField[]
  hpostdata?: Function
  component?: Component
  selected?: Data[]
  showAddBtn?: boolean
  data__title?: (data: Data) => string
  empty__data?: () => Data
  data__id?: (data: Data) => string
  data__desc?: (data: Data) => string
  tag?: any
  defaultSort?: string
  /** 列表表尾的数值聚合显示配置，透传给 TablePaged */
  aggregate?: AggregateConfig[]
}
