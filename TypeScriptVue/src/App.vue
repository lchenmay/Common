<template>
  <div class="dev-container">
    <h1>🧪 JCS Common</h1>

    <div>
      amt__2digitDollar: {{ amt__2digitDollar(-21223453245.2123) }}
    </div>

    <div>TabContainer</div>
    <div>
      <TabContainer />
    </div>

    <div>TablePaged</div>
    <div>
      <TablePaged 
        api="/api/admin/models"
      />
    </div>

    <div>SearchField</div>
    <div>
      <SearchField />
    </div>

    <div>Markdown</div>
    <div>
      <Markdown
        :markdown="md"
      />
    </div>


  <Crud 
    caption="物料清单"
    api="/api/admin/boms"
    :fields="[
    { key: 'ID', 
      sortable: true,
      style: 'table-cell-primary',
      text: 'left',
      width: '80px' },
    { key: '代码',
      width: '150px',
      text: 'left' },
    { key: '名称',
      width: '150px',
      text: 'left' },
    { key: '版本号',
      width: '150px',
      text: 'left' },
    { key: '上级',
      width: '150px',
      text: 'left' },
    { key: '成本(¥)',
      style: 'table-cell-amount',
      width: '120px',
      text: 'right' },
    { key: '说明',
      style: 'table-cell-text',
      width: '300px',
      text: 'left' }]"
  />

  </div>
</template>

<script setup lang="ts">

import * as vue from 'vue'

import TabContainer from './comps/TabContainer.vue'
import TablePaged from './comps/TablePaged.vue'
import SearchField from './comps/SearchField.vue'
import Markdown from './comps/Markdown.vue'
import { amt__2digitDollar } from './lib/text'

const md = String.raw`
1. 基准为线性参考模型 $y(x) = ax + b$
2. 需要至少两个数值确定线性参数，典型的包括停机/启动临界的下界，和最大工况的上界
3. 大部分情况考虑衰减，通常用指数衰减。以线性模型为理想模型，即上限；
用指数衰减 $\text e^\lambda x$ 来修正线性模型为保守模型
$y(x) = a\text e^\lambda x + b$，即下限；
`

vue.onMounted(async () => {
  console.log(md)
})

</script>

<style scoped>
.dev-container {
  padding: 2rem;
  max-width: 1200px;
  margin: 0 auto;
}
.test-section {
  border: 1px solid #e5e7eb;
  border-radius: 8px;
  padding: 1.5rem;
  margin: 1rem 0;
}
</style>