<template>


<div class="relative w-[400px]" :data-theme="theme">
    
    <input 
      type="text" 
      v-model="searchText"
      @focus="isDropdownVisible = true"
      @blur="onBlur"
      @input="onInput"
      placeholder="Start typing..."
      class="w-full border rounded px-4 py-3 focus:ring-2 focus:ring-blue-500 outline-none text-base"
    />

    <ul 
      v-if="isDropdownVisible && s.opts.length > 0"
      class="absolute z-10 bg-white border rounded shadow-lg mt-1 max-h-150 overflow-y-auto min-w-[400px] w-max max-w-none left-0">
      <li 
        v-for="opt in s.opts" 
        @click="onClick(opt)"
        :value="props.item__key(opt)"
        class="px-4 py-2 hover:bg-blue-50 cursor-pointer text-sm whitespace-nowrap">
        {{ props.item__text(opt) }}
      </li>
    </ul>
  </div>

</template>

<script setup lang="ts" generic="Data">

import * as vue from 'vue'
import { loader } from '~/lib/api'

const props = defineProps<{
  theme?: string
  api: string
  item__key: (item: Data) => string
  item__text: (item: Data) => string
  onselect: (item: Data) => void
}>()

const theme = vue.computed(() => props.theme || 'day')

const s = vue.shallowReactive({
  opts: [] as Data[]
})

const searchText = vue.ref('')
const isDropdownVisible = vue.ref(false)

const onInput = () => {
  if (!searchText.value) return

  loader(props.api, {
    term: searchText.value,
    act: "search"
  }, (rep: any) => {
    console.log(JSON.stringify(s.opts))
    s.opts = rep.data as Data[]
  },() => {
  })
}

const onClick = (opt: Data) => {
  searchText.value = props.item__text(opt)
  isDropdownVisible.value = false

  if(props.onselect)
    props.onselect(opt)
}

const onBlur = () => {
    setTimeout(() => isDropdownVisible.value = false, 200)    
}

</script>


<style scoped>
/* ===== Dark Theme ===== */
[data-theme="dark"] input {
  background-color: #1e293b;
  color: #e2e8f0;
  border-color: #334155;
}
[data-theme="dark"] input::placeholder {
  color: #64748b;
}
[data-theme="dark"] ul {
  background-color: #1e293b;
  border-color: #334155;
}
[data-theme="dark"] li {
  color: #e2e8f0;
}
[data-theme="dark"] li:hover {
  background-color: #1e3a5f;
}
</style>