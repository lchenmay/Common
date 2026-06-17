<template>

<div class="table-container text-left">
    <div class="table-card">
        
        <!-- 表格内容 -->
        <div class="table-wrapper">
            <table class="table">

              <thead class="table-header">
                    <tr class="table-row-border">
                        <th v-for="f in props.fields"
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
                    <tr v-for="i in s.items"
                      @click="props.onRowClick(i)"
                      class="table-row table-row-border">
                        <td v-for="f in props.fields"
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
        
        <!-- 分页栏 -->
        <div class="table-pagination">
            <div class="table-pagination-inner">
                
                <!-- 左侧：每页条数 -->
                <div class="table-page-size">
                    <select class="table-page-size-select"
                      @change="(e:Event) => { s.paging.npp = parseInt((e.target as HTMLSelectElement).value) }">
                        <option v-for="v in s.npps"
                          :selected="s.paging.npp === v">
                          {{ v }}  
                        </option>
                    </select>
                    <span class="table-page-size-label"> records per page</span>
                </div>
                
                <!-- 中间：页码按钮 -->
                <div class="table-pagination-buttons">
                    <button 
                      @click="loadPage(0)"
                      class="table-page-btn table-page-btn-disabled" disabled>First</button>
                    <button 
                      v-if="s.paging.page > 0"
                      @click="loadPage(s.paging.page - 1)"
                      class="table-page-btn table-page-btn-disabled" disabled>Previous</button>
                    
                    <span 
                      v-for="i in [-3,-2,-1,0,1,2,3]">
                      {{ i == 0 ? (s.paging.page + 1) : '' }}                      
                      <button
                        v-if="i != 0 && s.paging.page + i > 0 && s.paging.page + i <= s.paging.pages"
                        @click="loadPage(s.paging.page + i)">
                        {{ (i + 1) }}
                    </button>
                    </span>
                    
                    <button
                      v-if="s.paging.page < s.paging.pages" 
                      @click="loadPage(s.paging.page + 1)"
                      class="table-page-btn table-page-btn-inactive">Next</button>
                    <button 
                      @click="loadPage(s.paging.pages)"
                      class="table-page-btn table-page-btn-inactive">Last</button>
                </div>
                
                <!-- 右侧：统计信息 -->
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


<script setup lang="ts">

import * as vue from 'vue'
import { loader } from '~/lib/api'

export interface TableField {
  key: string,
  row__cell?: Function,
  row__style?: Function,
  sortable: boolean,
  style?: string,
  text?: string,
  width?: string
}

export interface Paging{
  npp: number,
  page: number,
  total: number,
  pages: number
}

const props = defineProps(['fields','api','hpostdata','onRowClick'])
props.fields as TableField[]
props.api as string
props.hpostdata as Function
props.onRowClick as Function

const s = vue.reactive({
  npps: [10,30,50,100,200],
  paging: {
    npp: 30,
    page: 0,
    total: 0,
    pages: 0
  } as Paging,
  items: [] as any[]
})

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
    s.items = rep.data
    s.paging = rep.paging
  })
}

vue.onMounted(async () => {
  loadPage(0)
})

</script>


