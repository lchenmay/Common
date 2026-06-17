<template>

  <SearchField 
    :api="props.api" 
    :item__key="props.data__id" 
    :item__text="props.data__desc"
    @select="openTab" />
  <TabContainer ref="tabRef" 
    :default-tab-type="'dashboard'" 
    :show-add-btn="true"
    @onClickCreate="openTab(props.empty__data())" />

</template>

<script setup lang="ts" generic="Data">

import * as vue from 'vue'

import { ref, markRaw, type Component } from 'vue'
import TablePaged, { type TableField } from './TablePaged.vue'
import TabContainer from './TabContainer.vue'
import SearchField from './SearchField.vue'

const props = defineProps([
  'caption',
  'api',
  'fields',
  'hpostdata',
  'component',
  'data__title','empty__data','data__id','data__desc'])
props.caption as string
props.api as string
props.fields as TableField[]
props.hpostdata as Function
props.component as Component
props.data__title as Function
props.empty__data as Function
props.data__id as Function
props.data__desc as Function

const tabRef = ref<InstanceType<typeof TabContainer>>()

const emits = defineEmits(['selected'])

const openTab = (i:Data) => {

    console.log("openTab", i)

    let id = props.data__id(i)
    tabRef.value?.createTab({
      id: id + '',
      type: 'VIEW',
      title: (id == 0 ? "新建" : (props.data__title(i))),
      component: markRaw(props.component),
      props:{
        data: i
      },
      closable: true
    })
}

vue.onMounted(async () => {
  tabRef.value?.createTab({
    id: 'LIST',
    type: 'LIST',
    title: props.caption,
    component: TablePaged,
    props: {
      fields: props.fields,
      api: props.api,
      hpostdata: props.hpostdata,
      onRowClick: openTab
    },
    closable: false
  })

})



</script>