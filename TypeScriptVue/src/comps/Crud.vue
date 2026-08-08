<template>

  <SearchField 
    :api="props.api!" 
    :item__key="props.data__id!" 
    :item__text="props.data__desc!" 
    :onselect="openTab" />

  <!-- 字段级筛选条：fields[].filter 显式配置优先，无则回退 probe 自动生成 -->
  <div v-if="filterBindings.length > 0" class="crud-filter-strip">
    <div v-for="b in filterBindings" :key="b.fieldKey" class="crud-filter-group">
      <span class="crud-filter-label">{{ b.fieldKey }}</span>
      <button
        v-for="opt in b.options"
        :key="String(opt.value)"
        class="crud-filter-chip"
        :class="chipClass(b, opt)"
        :style="chipStyle(b, opt)"
        @click="toggleFilter(b, opt.value)">
        {{ opt.label }}
      </button>
    </div>
  </div>

  <TabContainer ref="tabRef" :default-tab-type="'dashboard'" :show-add-btn="props.showAddBtn !== false"
    @onClickCreate="openTab(props.empty__data!())" />

</template>

<script setup lang="ts">

import * as vue from 'vue'

import { ref, markRaw, reactive } from 'vue'
import TablePaged from './TablePaged.vue'
import type { TableField, CrudProps } from './crud-types'
import TabContainer from './TabContainer.vue'
import SearchField from './SearchField.vue';
import { theme } from '../lib/common'
import { probeBindings, makeAutoRowStyle, makeFilterKey, makeValueGetter, type FieldBinding } from '../lib/orm/probe'

// 确保 theme 在 script 中被引用（TypeScript 不检查 template）
void theme.value

const props = defineProps<CrudProps>()

// theme 从 common.ts 导入（响应式 ref）
const tabRef = ref<InstanceType<typeof TabContainer>>()

// ---- 字段筛选：显式 filter 配置 + probe 自动兜底 ----
interface CrudFilterOption {
  value: any                // = 选项下标（字段枚举值）
  label: string
  style?: string | Record<string, string>
}
interface CrudFilterBinding {
  fieldKey: string
  options: CrudFilterOption[]
  get: (row: any) => any
  paramName: string
  applyRowStyle: boolean
  auto: boolean             // true = 来自 probe 自动探测（调色板上色）
}

/** 用 valuePath 在 empty__data 基行上构造"某枚举值"的合成行，供 row__cell 派生 label */
function syntheticRow(valuePath: string | undefined, val: any): any {
  const base = props.empty__data ? props.empty__data() : {}
  let clone: any
  try { clone = JSON.parse(JSON.stringify(base)) } catch { clone = { ...base } }
  if (!valuePath) return clone
  const ps = valuePath.split('.')
  let o = clone
  for (let k = 0; k < ps.length - 1; k++) {
    if (o[ps[k]] == null || typeof o[ps[k]] !== 'object') o[ps[k]] = {}
    o = o[ps[k]]
  }
  o[ps[ps.length - 1]] = val
  return clone
}

const filterBindings: CrudFilterBinding[] = reactive([]) as any
const filterSelections = reactive(new Map<string, Set<any>>())   // fieldKey -> selected value set
// 用 reactive 对象而非 ref：ref 经 TabContainer 的 v-bind 透传时会被自动解包成静态数字，
// 导致 TablePaged 的 watch 永远 watch 一个不变的值、筛选点击从不重载。
// reactive 普通对象不会被 v-bind 解包，且其 .n 属性能被 watch 正确追踪。
const filterTrigger = reactive({ n: 0 } as any)
let probedBindings: FieldBinding[] = []

function isFilterSelected(b: CrudFilterBinding, v: any): boolean {
  const sel = filterSelections.get(b.fieldKey)
  return !sel || sel.has(v)   // 空 Map = 全选
}
function toggleFilter(b: CrudFilterBinding, v: any) {
  let sel = filterSelections.get(b.fieldKey)
  if (!sel) {
    sel = new Set(b.options.map(opt => opt.value))
    filterSelections.set(b.fieldKey, sel)
  }
  if (sel.has(v)) {
    sel.delete(v)          // 允许取消到全部不选（= 不过滤，显示全部）
  } else {
    sel.add(v)
  }
  filterTrigger.n++
}
const CHIP_PALETTE = [
  '#6b7280','#2563eb','#059669','#d44005','#7c3aed','#0891b2',
  '#b45309','#db2777','#ca8a04','#0d9488','#16a34a','#dc2626',
]
function chipColor(b: CrudFilterBinding, v: any): string {
  const idx = b.options.findIndex(opt => opt.value === v)
  return CHIP_PALETTE[idx % CHIP_PALETTE.length] ?? '#6b7280'
}
/** 选中态：class 形式的 style 作为 class 应用；未选态淡出 */
function chipClass(b: CrudFilterBinding, opt: CrudFilterOption): string[] {
  const cls = ['crud-filter-chip']
  const on = isFilterSelected(b, opt.value)
  if (!on) cls.push('crud-filter-chip--off')
  if (on && !b.auto && typeof opt.style === 'string') cls.push(opt.style)
  return cls
}
/** 选中态：inline 对象直接返回；class 形式返回空（交由 class 处理）；未选态淡出 */
function chipStyle(b: CrudFilterBinding, opt: CrudFilterOption): Record<string, string> {
  const on = isFilterSelected(b, opt.value)
  if (!on) return { opacity: '0.4' }
  if (b.auto) return { backgroundColor: chipColor(b, opt.value), color: '#fff' }
  if (opt.style && typeof opt.style === 'object') return opt.style as Record<string, string>
  return {}
}

/** 把字段区的 filter 配置与 probe 自动探测归一成统一的筛选绑定结构 */
function buildFilterBindings(probed: FieldBinding[]): CrudFilterBinding[] {
  const out: CrudFilterBinding[] = []
  const fields = props.fields ?? []
  // 1) 显式 filter 配置优先
  for (const f of fields) {
    if (!f.filter) continue
    const pb = probed.find(b => b.fieldKey === f.key)
    const get = f.filter.get
      ?? (f.filter.valuePath
        ? (row: any) => f.filter!.valuePath!.split('.').reduce((o: any, k: string) => (o == null ? o : o[k]), row)
        : (pb ? makeValueGetter(pb) : (row: any) => (row as any)[f.key]))
    // 选项下标即 value（字段枚举值）；label 由同字段的 row__cell 派生
    const options = f.filter.options.map((o, idx) => ({
      value: idx,
      label: f.row__cell ? f.row__cell(syntheticRow(f.filter!.valuePath, idx)) : String(idx),
      style: o.style,
    }))
    out.push({
      fieldKey: f.key,
      options,
      get,
      paramName: f.filter.paramName
        ?? (pb ? makeFilterKey(pb) : f.key.toLowerCase() + 's'),
      applyRowStyle: f.filter.applyRowStyle ?? true,
      auto: false,
    })
  }
  // 2) 无 filter 的字段：沿用 probe 自动结果（调色板上色）
  for (const pb of probed) {
    if (fields.some(f => f.key === pb.fieldKey && f.filter)) continue
    out.push({
      fieldKey: pb.fieldKey,
      options: pb.values.map(v => ({ value: v.value, label: v.label })),
      get: makeValueGetter(pb),
      paramName: makeFilterKey(pb),
      applyRowStyle: true,
      auto: true,
    })
  }
  return out
}

/** 对有 filter 的列，把命中选项的 style 注入行单元格（class 名或 inline 对象） */
function buildAugmentedFields(): TableField[] {
  const bindings = filterBindings as any as CrudFilterBinding[]
  return (props.fields ?? []).map(f => {
    if (!f.filter) {
      // 无 filter：保留用户所给 row__style；否则尝试 probe 自动行样式（向后兼容）
      if (f.row__style) return f
      const auto = bindings.find(b => b.fieldKey === f.key && b.auto)
      if (auto) {
        const pb = probedBindings.find(b => b.fieldKey === f.key)
        if (pb) return { ...f, row__style: makeAutoRowStyle(pb) }
      }
      return f
    }
    // 有 filter 且 binding 要求 applyRowStyle：把命中选项的 style 注入行单元格
    // 注意：applyRowStyle 是 binding 上的字段（默认 true），不能读 f.filter.applyRowStyle
    // —— 输入字段通常没设该属性（undefined），会误判“不注入”导致列表 cell 永远无颜色。
    const fb = bindings.find(b => b.fieldKey === f.key)
    if (fb && !fb.applyRowStyle) return f
    const styleOf = (row: any) => {
      const v = bindings.find(b => b.fieldKey === f.key)!.get(row)
      const opt = f.filter!.options[Number(v)]
      return opt?.style
    }
    const row__style = (row: any) => {
      const s = styleOf(row)
      return (s && typeof s === 'string') ? s : ''
    }
    const row__style_inline = (row: any) => {
      const s = styleOf(row)
      return (s && typeof s === 'object') ? s : {}
    }
    return { ...f, row__style: f.row__style ?? row__style, row__style_inline }
  })
}

/** 包装 hpostdata：先调原始 hpostdata（如果存在），再注入筛选值 */
function wrapHpostdata(): Function | undefined {
  const bindings = filterBindings as any as CrudFilterBinding[]
  const orig = props.hpostdata
  return (t: any) => {
    if (orig) orig(t)
    for (const b of bindings) {
      const sel = filterSelections.get(b.fieldKey)
      if (!sel) continue
      // 直接发送选中集合：全不选 = []（后端精确匹配 0 条，即“没有数据”），
      // 部分选 = 子集，全选 = 全量。未交互（无 sel）则不发，后端视为“不过滤”。
      t[b.paramName] = [...sel]
    }
  }
}

const openTab = (i: any) => {
  let id = props.data__id!(i)
  tabRef.value?.createTab({
    id: id + '',
    type: 'VIEW',
    title: (id == '0' ? "新建" : (props.data__title!(i))),
    component: markRaw(props.component!),
    props: {
      data: i,
      tag: props.tag
    },
    closable: true
  })
}

vue.onMounted(async () => {
  // 探针：识别 fields 中绑定了哪些枚举/布尔字段（供 filter 配置回退 + 自动兜底）
  if (props.empty__data) {
    probedBindings = probeBindings(props.fields ?? [], props.empty__data)
    const bs = buildFilterBindings(probedBindings)
    ;(filterBindings as any).splice(0, filterBindings.length, ...bs)
  }

  // 应用字段筛选默认值：调用方给出要「预选」的选项下标集合；未列出的字段保持默认（全选）。
  // 用于 §3.4 场景：如 enabled 筛选默认仅预选 [1]（Enabled）→ 列表默认隐藏 disabled。
  if (props.filterDefaults) {
    for (const [fieldKey, idxs] of Object.entries(props.filterDefaults)) {
      filterSelections.set(fieldKey, new Set(idxs as any[]))
    }
  }

  const augmentedFields = buildAugmentedFields()

  tabRef.value?.createTab({
    id: 'LIST',
    type: 'LIST',
    title: props.caption!,
    component: TablePaged,
    props: {
      'lang': props.lang,
      fields: augmentedFields,
      api: props.api!,
      hpostdata: wrapHpostdata(),
      selected: props.selected,
      data__id: props.data__id,
      defaultSort: props.defaultSort,
      aggregate: props.aggregate,
      onRowClick: openTab,
      trigger: filterTrigger,
    },
    closable: false
  })

})


</script>

<style scoped>
.crud-filter-strip {
  padding: 0.75rem 1rem;
  margin-bottom: 1rem;
  background: #f9fafb;
  border: 1px solid #e5e7eb;
  border-radius: 0.5rem;
}
.crud-filter-group {
  /* 块级容器 + inline-block 子元素：避开 base.css 把所有 button 设成 display:flex/width:100% 的坑 */
  margin-bottom: 0.5rem;
  line-height: 1.75rem;
}
.crud-filter-group:last-child {
  margin-bottom: 0;
}
.crud-filter-label {
  display: inline-block;
  font-size: 0.875rem;
  font-weight: 500;
  color: #374151;
  margin-right: 0.5rem;
  min-width: 5rem;
  vertical-align: middle;
}
/* 用 .crud-filter-group 前缀把特异性提到 (0,3,0)，确保压过 base.css 全局 button 的 display:flex */
.crud-filter-group .crud-filter-chip {
  /* 覆盖 base.css 全局 button 的 flex/width:100%/min-w-[60px]/flex-shrink-0/mx-2，让 chip 内联 */
  display: inline-block !important;
  flex: none !important;
  width: auto !important;
  min-width: 0 !important;
  margin: 0 0.25rem 0.25rem 0 !important;
  padding: 0.25rem 0.625rem !important;
  font-size: 0.875rem;
  line-height: 1.25rem;
  border: 1px solid transparent;
  border-radius: 0.375rem;
  background: #fff;
  color: #1f2937;
  cursor: pointer;
  white-space: nowrap;
  text-align: center;
  vertical-align: middle;
  transition: opacity 0.2s, transform 0.2s;
}
.crud-filter-chip:hover {
  opacity: 0.85;
  transform: scale(1.05);
}
.crud-filter-chip--off {
  opacity: 0.4;
}

/* dark */
[data-theme="dark"] .crud-filter-strip {
  background: #0f172a;
  border-color: #1e293b;
}
[data-theme="dark"] .crud-filter-label {
  color: #cbd5e1;
}
[data-theme="dark"] .crud-filter-chip {
  background: #1e293b;
  color: #e2e8f0;
}
</style>