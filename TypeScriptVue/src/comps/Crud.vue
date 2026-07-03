<template>

  <SearchField 
    :api="props.api!" 
    :item__key="props.data__id!" 
    :item__text="props.data__desc!" 
    :onselect="openTab" />
  <TabContainer ref="tabRef" :default-tab-type="'dashboard'" :show-add-btn="props.showAddBtn !== false"
    @onClickCreate="openTab(props.empty__data!())" />

</template>

<script setup lang="ts" generic="Data">

import * as vue from 'vue'

import { ref, markRaw, type Component } from 'vue'
import TablePaged, { type TableField } from './TablePaged.vue'
import TabContainer from './TabContainer.vue'
import SearchField from './SearchField.vue';
import { theme } from '../lib/common'

// 确保 theme 在 script 中被引用（TypeScript 不检查 template）
void theme.value

interface CrudProps {
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
}

const props = defineProps<CrudProps>()

// theme 从 common.ts 导入（响应式 ref）
const tabRef = ref<InstanceType<typeof TabContainer>>()

const openTab = (i: Data) => {
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

  tabRef.value?.createTab({
    id: 'LIST',
    type: 'LIST',
    title: props.caption!,
    component: TablePaged,
    props: {
      'lang': props.lang,
      fields: props.fields!,
      api: props.api!,
      hpostdata: props.hpostdata,
      selected: props.selected,
      onRowClick: openTab
    },
    closable: false
  })

})


</script>