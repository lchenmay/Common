<template>


<div class="relative w-[400px]">
    
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
        @click="selectOption(opt)"
        :value="props.item__key(opt)"
        class="px-4 py-2 hover:bg-blue-50 cursor-pointer text-sm whitespace-nowrap"
      >
        {{ props.item__text(opt) }}
      </li>
    </ul>
  </div>

</template>

<script setup lang="ts">

import * as vue from 'vue'
import { loader } from '~/lib/api'

const props = defineProps(['api','item__key','item__text'])
props.api as string
props.item__key as Function
props.item__text as Function

const s = vue.reactive({
  opts: []
})

const emits = defineEmits(['select']) 

const searchText = vue.ref('')
const isDropdownVisible = vue.ref(false)

const onInput = () => {
  if (!searchText.value) return

  loader(props.api, {
    term: searchText.value,
    act: "search"
  }, (rep: any) => {
    s.opts = rep.data
    console.log(s.opts)
  },() => {
  })
}

const selectOption = (opt: any) => {
  searchText.value = props.item__key(opt)
  isDropdownVisible.value = false

  emits('select',opt)
}

const onBlur = () => {
    setTimeout(() => isDropdownVisible.value = false, 200)    
}

</script>