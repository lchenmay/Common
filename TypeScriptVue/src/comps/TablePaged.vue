<template>

<div class="table-container text-left">
    <div class="table-card">
        
        <div class="table-wrapper">
            <table class="table">

              <thead class="table-header">
                    <tr class="table-row-border">

                        <th class="table-header-cell text-center" style="width: 50px">
                          <div v-if="props.selected">
                            <input 
                              type="checkbox" 
                              :checked="isAllSelected" 
                              :indeterminate="isIndeterminate"
                              @change="toggleAll"
                              class="cursor-pointer"
                            />
                          </div>
                        </th>

                        <th v-for="f in props.fields"
                            :key="f.key"
                            class="table-header-cell"
                            :style="[
                              f.width ? { width: f.width, textAlign: f.text === 'right' ? 'right' : 'left' } : 
                              { textAlign: f.text === 'right' ? 'right' : 'left' }]"
                            :class="buildStyleTh(f)"
                            @click="f.sortable ? handleSort(f.key) : undefined">
                            {{ f.key }}
                            <span v-if="f.sortable" class="table-sort-icon" :class="getSortIconClass(f.key)">
                              <span v-if="getSortDirection(f.key) === 'asc'">↑</span>
                              <span v-else-if="getSortDirection(f.key) === 'desc'">↓</span>
                              <span v-else>⇅</span>
                            </span>
                        </th>
                    </tr>
                </thead>
                
                <tbody>
                    <tr v-for="(i, index) in s.items"
                      :key="index"
                      @click="props.onRowClick(i)"
                      class="table-row table-row-border cursor-pointer hover:bg-slate-50">

                        <td class="table-cell text-center" style="width: 50px">
                          <div v-if="props.selected" @click.stop>
                            <input 
                              type="checkbox" 
                              :checked="isRowSelected(i)" 
                              @change="toggleRow(i)"
                              class="cursor-pointer"
                            />
                          </div>
                        </td>

                        <td v-for="f in props.fields"
                          :key="f.key"
                          :style="[
                            f.width ? { width: f.width, textAlign: f.text === 'right' ? 'right' : 'left' } : 
                            { textAlign: f.text === 'right' ? 'right' : 'left' }]"
                          :class="buildStyleTd(f)(i)">
                          <span v-if="f.row__cell">
                            {{ f.row__cell(i) }}
                          </span>
                        </td>
                    </tr>
                </tbody>
            </table>
        </div>
        
        <div class="table-pagination">
            <div class="table-pagination-inner">
                
                <div class="table-page-size">
                    <select class="table-page-size-select"
                      @change="(e:Event) => { s.paging.npp = parseInt((e.target as HTMLSelectElement).value); loadPage(0); }">
                        <option v-for="v in s.npps"
                          :key="v"
                          :value="v"
                          :selected="s.paging.npp === v">
                          {{ v }}  
                        </option>
                    </select>
                    <span class="table-page-size-label">
                      {{ lang__display('rpp') }} 
                    </span>
                </div>
                
                <div class="table-pagination-buttons">
                    <button 
                      @click="loadPage(0)"
                      class="table-page-btn" :disabled="s.paging.page === 0">
                      {{ lang__display('fst') }} 
                    </button>
                    <button 
                      @click="loadPage(s.paging.page - 1)"
                      class="table-page-btn" :disabled="s.paging.page === 0">
                      {{ lang__display('prv') }} 
                    </button>
                    
                    <span v-for="i in [-3,-2,-1,0,1,2,3]" :key="i">
                      <span v-if="i == 0" class="mx-2 font-bold">{{ s.paging.page + 1 }}</span>                      
                      <button
                        v-if="i != 0 && s.paging.page + i >= 0 && s.paging.page + i < s.paging.pages"
                        @click="loadPage(s.paging.page + i)"
                        class="table-page-btn mx-1">
                        {{ s.paging.page + i + 1 }}
                      </button>
                    </span>
                    
                    <button
                      @click="loadPage(s.paging.page + 1)" 
                      class="table-page-btn" :disabled="s.paging.page >= s.paging.pages - 1">
                      {{ lang__display('nxt') }} 
                    </button>
                    <button 
                      @click="loadPage(s.paging.pages - 1)"
                      class="table-page-btn" :disabled="s.paging.page >= s.paging.pages - 1">
                      {{ lang__display('lst') }} 
                    </button>
                </div>
                
                <div class="table-stats">
                    {{ lang__display('total') }} 
                    <span class="table-stats-number">
                      {{ s.paging.total.toLocaleString('en-US') }}  
                    </span>.
                    {{ lang__display('current') }} 
                    <span class="table-stats-number">
                      {{ (s.paging.page + 1).toLocaleString('en-US') }}  
                    </span> / <span class="table-stats-number">
                      {{ s.paging.pages.toLocaleString('en-US') }}  
                    </span>.
                </div>
            </div>
        </div>
        
    </div>
</div>

</template>


<script setup lang="ts" generic="Data">

import * as vue from 'vue'
import { computed } from 'vue'
import { loader } from '~/lib/api'

export interface TableField {
  key: string,
  row__cell?: Function,
  row__style?: Function,
  sortable: boolean,
  style?: Function,
  text?: string,
  width?: string
}

export interface Paging{
  npp: number,
  page: number,
  total: number,
  pages: number
}

const props = defineProps(['lang','fields','api','hpostdata','onRowClick','selected'])
props.lang as string
props.fields as TableField[]
props.api as string
props.hpostdata as Function
props.onRowClick as Function
props.selected as Data[] | undefined

const s = vue.shallowReactive({
  npps: [10,30,50,100,200],
  paging: {
    npp: 30,
    page: 0,
    total: 0,
    pages: 0
  } as Paging,
  items: [] as Data[],
  sort: '', // 排序字段，格式: '+field' 或 '-field'
  sortField: '', // 当前排序的字段名
  sortDirection: '' as '' | 'asc' | 'desc', // 排序方向
  trigger: 0
})

const lang__display = (display:string) => {
  if(props.lang && props.lang == 'zh'){
    switch(display){
      case 'rpp': return '记录/页'
      case 'fst': return '首页'
      case 'prv': return '上页'
      case 'nxt': return '下页'
      case 'lst': return '末页'
      case 'total': return '总页数'
      case 'current': return '当前'
      default: return ''
    }
  }
  else{
    switch(display){
      case 'rpp': return 'records per page'
      case 'fst': return 'First'
      case 'prv': return 'Previous'
      case 'nxt': return 'Next'
      case 'lst': return 'Last'
      case 'total': return 'Total: '
      case 'current': return 'Current: '
      default: return ''
    }
  }
}

// 获取排序方向
const getSortDirection = (fieldKey: string): '' | 'asc' | 'desc' => {
  if (s.sortField === fieldKey) {
    return s.sortDirection
  }
  return ''
}

// 获取排序图标样式
const getSortIconClass = (fieldKey: string): string => {
  if (s.sortField === fieldKey) {
    return 'sort-active'
  }
  return 'sort-inactive'
}

// 处理排序点击
const handleSort = (fieldKey: string) => {
  // 点击同一个字段：切换排序方向
  if (s.sortField === fieldKey) {
    if (s.sortDirection === 'asc') {
      // 升序 → 降序
      s.sortDirection = 'desc'
      s.sort = '-' + fieldKey
    } else if (s.sortDirection === 'desc') {
      // 降序 → 不排序
      s.sortDirection = ''
      s.sort = ''
      s.sortField = ''
      // 触发加载
      loadPage(0)
      return
    }
  } else {
    // 点击不同字段：默认升序
    s.sortField = fieldKey
    s.sortDirection = 'asc'
    s.sort = '+' + fieldKey
  }
  
  // 触发加载
  loadPage(0)
}

// 判定某行对象是否被多选勾选
const isRowSelected = (item: Data): boolean => {
  if (!props.selected) return false
  return s.trigger >= 0 && props.selected.includes(item)
}

// 判定当前页数据是否处于全选状态
const isAllSelected = computed(() => {
  if (!props.selected || s.items.length === 0) return false
  return s.trigger >= 0 && s.items.every(item => isRowSelected(item))
})

// 判定半选状态
const isIndeterminate = computed(() => {
  if (!props.selected || s.items.length === 0) return false
  if (isAllSelected.value) return false
  return s.trigger >= 0 && s.items.some(item => isRowSelected(item))
})

// 改变单行勾选状态
const toggleRow = (item: Data) => {
  if (!props.selected) return

  const idx = props.selected.indexOf(item)
  if (idx !== -1) {
    props.selected.splice(idx, 1)
  } else {
    props.selected.push(item)
  }
  s.trigger++
}

// 改变全选状态
const toggleAll = () => {
  if (!props.selected) return

  if (isAllSelected.value) {
    s.items.forEach(item => {
      const idx = props.selected.indexOf(item)
      if (idx !== -1) {
        props.selected.splice(idx, 1)
      }
    })
  } else {
    s.items.forEach(item => {
      if (!isRowSelected(item)) {
        props.selected.push(item)
      }
    })
  }
  s.trigger++
}

const buildStyleTh = (f:TableField) => {
  const classes = ['table-cell']
  if(f.sortable)
    classes.push('table-header-cell-sortable')
  if(f.text)
    classes.push('text-' + f.text)
  return classes
}

const buildStyleTd = (f:TableField) => (row:any) => {
  const classes = ['table-cell']
  if(f.row__style)
    classes.push(f.row__style(row))
  if(f.text)
    classes.push('text-' + f.text)
  return classes
}

const loadPage = (page:number) => {
  s.paging.page = page

  let postdata = {
    paging: s.paging,
    sort: s.sort,
    act: "ls"
  }

  if(props.hpostdata)
    props.hpostdata(postdata)

  loader(props.api, postdata, (rep: any) => {
    s.items = rep.data as Data[]
    s.paging = rep.paging
    s.trigger++ 
  })
}

vue.onMounted(async () => {
  loadPage(0)
})

</script>


<style scoped>
/* 排序图标样式 */
.table-header-cell-sortable {
  cursor: pointer;
  user-select: none;
}

.table-sort-icon {
  display: inline-block;
  margin-left: 4px;
  font-size: 0.75rem;
  transition: color 0.2s;
}

.table-sort-icon.sort-active {
  color: #3b82f6;
  font-weight: bold;
}

.table-sort-icon.sort-inactive {
  color: #9ca3af;
}

.table-header-cell-sortable:hover .table-sort-icon.sort-inactive {
  color: #6b7280;
}

/* 原有样式 */
.table-container {
  @apply w-full;
}

.table-card {
  @apply bg-white rounded-lg shadow overflow-hidden;
}

.table-wrapper {
  @apply overflow-x-auto;
}

.table {
  @apply w-full text-sm;
}

.table-header {
  @apply bg-gray-50;
}

.table-header-cell {
  @apply px-4 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider;
}

.table-header-cell-sortable {
  @apply cursor-pointer hover:bg-gray-100;
}

.table-row-border {
  @apply border-b border-gray-200;
}

.table-row {
  @apply hover:bg-gray-50 transition-colors duration-150;
}

.table-cell {
  @apply px-4 py-3 text-sm text-gray-900;
}

.table-pagination {
  @apply px-4 py-3 bg-white border-t border-gray-200;
}

.table-pagination-inner {
  @apply flex items-center justify-between flex-wrap gap-2;
}

.table-page-size {
  @apply flex items-center gap-2;
}

.table-page-size-select {
  @apply border border-gray-300 rounded px-2 py-1 text-sm focus:outline-none focus:ring-2 focus:ring-blue-500;
}

.table-page-size-label {
  @apply text-sm text-gray-600;
}

.table-pagination-buttons {
  @apply flex items-center gap-1;
}

.table-page-btn {
  @apply px-3 py-1 text-sm border border-gray-300 rounded hover:bg-gray-50 disabled:opacity-50 disabled:cursor-not-allowed transition-colors duration-150;
}

.table-page-btn:disabled {
  @apply cursor-not-allowed opacity-50;
}

.table-stats {
  @apply text-sm text-gray-600;
}

.table-stats-number {
  @apply font-medium text-gray-800;
}
</style>