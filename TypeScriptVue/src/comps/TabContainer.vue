<template>
  <div class="tab-container" :data-theme="theme">
    <!-- 标签栏 -->
    <div class="tab-bar">
      <div class="tab-list">
        <div v-for="tab in tabs" 
             :key="tab.id"
             class="tab-item"
             :class="{ 'tab-item-active': activeTabId === tab.id }"
             @click="activateTab(tab.id)">
          <span class="tab-title">{{ tab.title }}</span>
          <button v-if="tab.closable !== false" 
                  class="tab-close-btn"
                  @click.stop="closeTab(tab.id)">
            <svg class="tab-close-icon" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
              <path d="M18 6L6 18M6 6l12 12"/>
            </svg>
          </button>
        </div>
      </div>
      
      <!-- 可选：添加按钮 -->
      <button v-if="showAddBtn" class="tab-add-btn" @click="emit('onClickCreate')">
        <svg class="tab-add-icon" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
          <path d="M12 5v14M5 12h14"/>
        </svg>
      </button>
    </div>
    
    <!-- 内容区域 -->
    <div class="tab-content">
      <component :is="activeTabComponent" 
                 v-if="activeTabComponent"
                 v-bind="activeTabProps" />
      <div v-else class="tab-empty">
        
      </div>
    </div>
  </div>
</template>

<script setup lang="ts">

import { ref, computed, watch, type Component } from 'vue'

// 标签项接口
export interface TabItem {
  id: string
  title: string
  type: string           // 标签类型，用于去重
  component: Component   // 对应的组件
  props?: Record<string, any>  // 传递给组件的 props
  closable?: boolean     // 是否可关闭，默认 true
  data?: any             // 额外数据
}

// Props
const props = defineProps<{
  theme?: string
  initialTabs?: TabItem[]
  defaultTabType?: string
  defaultTabTitle?: string
  defaultTabComponent?: Component
  showAddBtn?: boolean
}>()

// Emits
const theme = computed(() => props.theme || 'day')

const emit = defineEmits<{
  (e: 'onClickCreate'): void
  (e: 'tab-created', tab: TabItem): void
  (e: 'tab-closed', tab: TabItem): void
  (e: 'tab-activated', tab: TabItem): void
  (e: 'tabs-changed', tabs: TabItem[]): void
}>()

// 状态
const tabs = ref<TabItem[]>(props.initialTabs || [])
const activeTabId = ref<string>('')

// 计算属性
const activeTab = computed(() => tabs.value.find(t => t.id === activeTabId.value))
const activeTabComponent = computed(() => activeTab.value?.component)
const activeTabProps = computed(() => activeTab.value?.props || {})

// 获取指定类型的标签
const getTabByTypeId = (type: string, id: string): TabItem | undefined => {
  return tabs.value.find(tab => 
    tab.type === type && tab.id === id)
}

// 创建标签（如果已存在则激活）
const createTab = (tab: TabItem): TabItem | null => {

  const existingTab = getTabByTypeId(tab.type, tab.id)
  if (existingTab) {
    activateTab(existingTab.id)
    return existingTab
  }
  
  // 生成唯一 ID
  const newTab = {
    ...tab,
    id: tab.id || `${tab.type}_${Date.now()}`
  }
  
  tabs.value.push(newTab)
  activateTab(newTab.id)
  emit('tab-created', newTab)
  emit('tabs-changed', tabs.value)
  
  return newTab
}

// 根据类型创建标签（使用默认配置）
const createTabByType = (
  type: string, 
  title: string, 
  component: Component, 
  props?: Record<string, any>,
  closable?: boolean
): TabItem | null => {
  return createTab({
    id: `${type}_${Date.now()}`,
    type,
    title,
    component,
    props,
    closable
  })
}

// 创建默认标签
const createDefaultTab = () => {
  if (props.defaultTabType && props.defaultTabComponent) {
    createTabByType(
      props.defaultTabType,
      props.defaultTabTitle || '新标签',
      props.defaultTabComponent
    )
  }
}

// 激活标签
const activateTab = (id: string) => {
  const tab = tabs.value.find(t => t.id === id)
  if (!tab) return
  
  activeTabId.value = id
  emit('tab-activated', tab)
}

// 关闭标签
const closeTab = (id: string) => {
  const index = tabs.value.findIndex(t => t.id === id)
  if (index === -1) return
  
  const tab = tabs.value[index]
  
  // 不允许关闭不可关闭的标签
  if (tab.closable === false) return
  
  tabs.value.splice(index, 1)
  emit('tab-closed', tab)
  emit('tabs-changed', tabs.value)
  
  // 如果关闭的是当前激活的标签
  if (activeTabId.value === id) {
    if (tabs.value.length > 0) {
      // 激活相邻的标签
      const newIndex = Math.min(index, tabs.value.length - 1)
      activeTabId.value = tabs.value[newIndex].id
      emit('tab-activated', tabs.value[newIndex])
    } else {
      activeTabId.value = ''
    }
  }
}

// 更新标签标题
const updateTabTitle = (id: string, title: string) => {
  const tab = tabs.value.find(t => t.id === id)
  if (tab) {
    tab.title = title
    emit('tabs-changed', tabs.value)
  }
}

// 更新标签 Props
const updateTabProps = (id: string, props: Record<string, any>) => {
  const tab = tabs.value.find(t => t.id === id)
  if (tab) {
    tab.props = { ...tab.props, ...props }
  }
}

// 设置当前激活标签的数据
const setActiveTabData = (data: any) => {
  if (activeTab.value) {
    activeTab.value.data = data
  }
}

// 获取当前激活标签的数据
const getActiveTabData = () => {
  return activeTab.value?.data
}

// 关闭所有标签
const closeAllTabs = () => {
  // 保留不可关闭的标签
  const closableTabs = tabs.value.filter(t => t.closable !== false)
  const fixedTabs = tabs.value.filter(t => t.closable === false)
  
  closableTabs.forEach(tab => {
    emit('tab-closed', tab)
  })
  
  tabs.value = [...fixedTabs]
  emit('tabs-changed', tabs.value)
  
  if (tabs.value.length > 0) {
    activeTabId.value = tabs.value[0].id
    emit('tab-activated', tabs.value[0])
  } else {
    activeTabId.value = ''
  }
}

// 获取当前激活的标签
const getActiveTab = () => {
  return activeTab.value
}

// 暴露方法给父组件
defineExpose({
  createTab,
  createTabByType,
  closeTab,
  closeAllTabs,
  activateTab,
  updateTabTitle,
  updateTabProps,
  getActiveTab, 
  getActiveTabData,
  setActiveTabData,
  tabs,
  activeTabId
})


// 初始化：如果没有标签且设置了默认标签，自动创建
watch(() => props.initialTabs, (newTabs) => {
  if (newTabs && newTabs.length > 0) {
    tabs.value = [...newTabs]
    if (newTabs.length > 0 && !activeTabId.value) {
      activeTabId.value = newTabs[0].id
    }
  }
}, { immediate: true })

// 如果没有标签且有默认配置，自动创建
if (tabs.value.length === 0 && props.defaultTabType && props.defaultTabComponent) {
  createDefaultTab()
}
</script>


<style scoped>
.tab-container {
  display: flex;
  flex-direction: column;
  height: 100%;
}
.tab-bar {
  display: flex;
  align-items: center;
  background: #f8fafc;
  border-bottom: 1px solid #e2e8f0;
  padding: 0 8px;
  min-height: 40px;
  gap: 4px;
}
.tab-list {
  display: flex;
  flex: 1;
  overflow-x: auto;
  gap: 2px;
}
.tab-item {
  display: flex;
  align-items: center;
  padding: 6px 12px;
  border-radius: 6px 6px 0 0;
  font-size: 13px;
  color: #64748b;
  cursor: pointer;
  border: 1px solid transparent;
  border-bottom: none;
  transition: all 0.15s;
  white-space: nowrap;
  user-select: none;
}
.tab-item:hover {
  background: #f1f5f9;
  color: #334155;
}
.tab-item-active {
  background: #fff;
  color: #1e293b;
  font-weight: 500;
  border-color: #e2e8f0;
}
.tab-title {
  max-width: 160px;
  overflow: hidden;
  text-overflow: ellipsis;
}
.tab-close-btn {
  display: flex;
  align-items: center;
  justify-content: center;
  width: 18px;
  height: 18px;
  margin-left: 6px;
  border: none;
  background: transparent;
  color: #94a3b8;
  border-radius: 4px;
  cursor: pointer;
  transition: all 0.1s;
}
.tab-close-btn:hover {
  background: #e2e8f0;
  color: #64748b;
}
.tab-close-icon {
  width: 12px;
  height: 12px;
}
.tab-add-btn {
  display: flex;
  align-items: center;
  justify-content: center;
  width: 28px;
  height: 28px;
  border: none;
  background: transparent;
  color: #64748b;
  border-radius: 6px;
  cursor: pointer;
  transition: all 0.15s;
  flex-shrink: 0;
}
.tab-add-btn:hover {
  background: #e2e8f0;
  color: #1e293b;
}
.tab-add-icon {
  width: 16px;
  height: 16px;
}
.tab-content {
  flex: 1;
  overflow: auto;
  background: #fff;
}
.tab-empty {
  display: flex;
  align-items: center;
  justify-content: center;
  height: 100%;
  color: #94a3b8;
}

/* ===== Dark Theme ===== */
[data-theme="dark"] .tab-bar {
  background: #0f172a;
  border-bottom: 1px solid #334155;
}
[data-theme="dark"] .tab-item {
  color: #94a3b8;
}
[data-theme="dark"] .tab-item:hover {
  background: #1e293b;
  color: #cbd5e1;
}
[data-theme="dark"] .tab-item-active {
  background: #1e293b;
  color: #e2e8f0;
  border-color: #334155;
}
[data-theme="dark"] .tab-close-btn {
  color: #64748b;
}
[data-theme="dark"] .tab-close-btn:hover {
  background: #334155;
  color: #94a3b8;
}
[data-theme="dark"] .tab-add-btn {
  color: #94a3b8;
}
[data-theme="dark"] .tab-add-btn:hover {
  background: #334155;
  color: #e2e8f0;
}
[data-theme="dark"] .tab-content {
  background: #1e293b;
}
</style>