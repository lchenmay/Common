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
import { amt__2digitDollar } from './lib/text'

vue.onMounted(async () => {
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