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
                            :class="buildStyleTh(f)">                      
                            {{ f.key }}
                            <span v-if="f.sortable" class="table-sort-icon">↓</span>
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
                    <span class="table-page-size-label"> records per page</span>
                </div>
                
                <div class="table-pagination-buttons">
                    <button 
                      @click="loadPage(0)"
                      class="table-page-btn" :disabled="s.paging.page === 0">First</button>
                    <button 
                      @click="loadPage(s.paging.page - 1)"
                      class="table-page-btn" :disabled="s.paging.page === 0">Previous</button>
                    
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
                      class="table-page-btn" :disabled="s.paging.page >= s.paging.pages - 1">Next</button>
                    <button 
                      @click="loadPage(s.paging.pages - 1)"
                      class="table-page-btn" :disabled="s.paging.page >= s.paging.pages - 1">Last</button>
                </div>
                
                <div class="table-stats">
                    Total: 
                    <span class="table-stats-number">
                      {{ s.paging.total.toLocaleString('en-US') }}  
                    </span>.
                    Page: 
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

const props = defineProps(['fields','api','hpostdata','onRowClick','selected'])
props.fields as TableField[]
props.api as string
props.hpostdata as Function
props.onRowClick as Function
props.selected as Data[] | undefined

// 在 Vue 3 的 reactive 中，泛型数组 Data[] 会被 Vue 的响应式系统包装，导致类型推断为 UnwrapRefSimple<Data>[] 而不是原始的 Data[]。
//const s = vue.reactive({
const s = vue.shallowReactive({
  npps: [10,30,50,100,200],
  paging: {
    npp: 30,
    page: 0,
    total: 0,
    pages: 0
  } as Paging,
  items: [] as Data[],
  trigger: 0 // 轻量响应式计数器，专门用来驱动直接修改数组引用内部的值时的视图联动
})

// 判定某行对象是否被多选勾选
const isRowSelected = (item: Data): boolean => {
  if (!props.selected) return false
  // 依赖 s.trigger。当 trigger 改变时，此处的判断函数会被强制重新运行
  return s.trigger >= 0 && props.selected.includes(item)
}

// 判定当前页数据是否处于全选状态
const isAllSelected = computed(() => {
  if (!props.selected || s.items.length === 0) return false
  return s.trigger >= 0 && s.items.every(item => isRowSelected(item))
})

// 判定半选状态（当前页仅部分行勾选）
const isIndeterminate = computed(() => {
  if (!props.selected || s.items.length === 0) return false
  if (isAllSelected.value) return false
  return s.trigger >= 0 && s.items.some(item => isRowSelected(item))
})

// 改变单行勾选状态：直接操作 props.selected 原数组进行 push / splice 剪切
const toggleRow = (item: Data) => {
  if (!props.selected) return

  const idx = props.selected.indexOf(item)
  if (idx !== -1) {
    props.selected.splice(idx, 1) // 存在则直接剔除
  } else {
    props.selected.push(item)     // 不存在则执行调用你所期望的 .push()
  }
  s.trigger++ // 触发依赖更新，让 checkbox 的对勾亮起或熄灭
}

// 改变全选状态：批量进行 push / 就地移除
const toggleAll = () => {
  if (!props.selected) return

  if (isAllSelected.value) {
    // 如果当前页已经全选，点击时把当前页所有的元素从 props.selected 里就地拿掉
    s.items.forEach(item => {
      const idx = props.selected.indexOf(item)
      if (idx !== -1) {
        props.selected.splice(idx, 1)
      }
    })
  } else {
    // 如果没有全选，把当前页里还没勾选的对象 push 进去
    s.items.forEach(item => {
      if (!isRowSelected(item)) {
        props.selected.push(item)
      }
    })
  }
  s.trigger++ // 触发依赖更新
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