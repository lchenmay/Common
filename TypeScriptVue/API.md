# @lchenmay/jcs-common API 文档

> 版本: `1.0.139` | 更新: 2026-06-25

---

## 一、Vue 组件

### 1. FileUploader — 文件上传器

拖拽/点击上传组件，支持缩略图预览、XHR 进度条、中英双语。

```vue
<FileUploader
  lang="zh"
  uploadUrl="/api/public/upload"
  baseUrl="https://api.example.com"
  accept="image/*,.pdf"
  :autoUpload="false"
  :maxSize="1073741824"
  @uploaded="onFileUploaded"
  @error="onFileError"
/>
```

| Prop | 类型 | 默认值 | 说明 |
|------|------|--------|------|
| `lang` | `string` | — | `'zh'` \| `'en'`，控制界面语言 |
| `uploadUrl` | `string` | `'/api/public/upload'` | 上传目标 URL |
| `baseUrl` | `string` | — | 后端基础 URL，用于拼接 `/thumbnail/{id}` |
| `accept` | `string` | — | 接受的文件类型，如 `'image/*,.pdf'` |
| `multiple` | `boolean` | `true` | 是否允许多选 |
| `autoUpload` | `boolean` | `false` | 选择文件后自动上传 |
| `maxSize` | `number` | 10GB | 单文件最大字节数 |
| `headers` | `Record<string, string>` | — | 额外的请求头 |

| Event | 参数 | 说明 |
|-------|------|------|
| `uploaded` | `file: FileItem` | 单个文件上传成功 |
| `error` | `file: FileItem, err: string` | 单个文件上传失败 |

| 暴露方法 | 说明 |
|----------|------|
| `files` (reactive) | 响应式文件列表 `FileItem[]` |
| `uploadAll()` | 全部上传 |
| `remove(idx)` | 按索引移除文件 |

---

### 2. BatchUploader — 批量文件上传器

深色主题的批量上传组件，支持并发上传（3个并发）、认证 token、自定义 FormData 字段。

```vue
<BatchUploader
  uploadUrl="/api/admin/upload"
  :extraFields="{ projectCode: 'WYI' }"
  authToken="Bearer xxx"
  accept="image/*,.pdf,.docx"
  acceptHint="支持图片、PDF、Word 文档"
  dropText="拖拽账单文件到此处"
  :autoUpload="true"
  @fileDone="onFileDone"
  @fileError="onFileError"
  @allDone="onAllDone"
/>
```

| Prop | 类型 | 默认值 | 说明 |
|------|------|--------|------|
| `uploadUrl` | `string` | `'/api/public/upload'` | 上传目标 URL |
| `extraFields` | `Record<string, string>` | — | 额外的 FormData 字段 |
| `authToken` | `string` | — | Bearer 认证 token |
| `accept` | `string` | — | 接受的文件类型 |
| `acceptHint` | `string` | — | 类型提示文字 |
| `dropText` | `string` | — | 拖拽区提示文字 |
| `maxSize` | `number` | 无限制 | 单文件最大字节数 |
| `autoUpload` | `boolean` | `false` | 自动上传 |

| Event | 参数 | 说明 |
|-------|------|------|
| `fileDone` | `task: UploadTask` | 单个文件上传完成 |
| `fileError` | `task: UploadTask` | 单个文件上传失败 |
| `allDone` | `tasks: UploadTask[]` | 全部上传完成 |

**导出类型**:
```ts
export interface UploadTask {
  id: string
  file: File
  progress: number
  status: 'idle' | 'uploading' | 'success' | 'error'
  message?: string
  rep?: any
}
export interface UploaderProps { /* 上述 Props 类型 */ }
```

---

### 3. FileSysTree — 文件系统树

递归文件/文件夹树组件，支持懒加载和传入静态数据。

```vue
<!-- 方式1: 通过 API 加载 -->
<FileSysTree
  lang="zh"
  baseUrl="https://api.example.com"
  api="/api/admin/listDir"
  @select="onFileSelect"
/>

<!-- 方式2: 直接传入节点 -->
<FileSysTree
  :nodes="fileTreeData"
  @select="onFileSelect"
/>
```

| Prop | 类型 | 默认值 | 说明 |
|------|------|--------|------|
| `lang` | `string` | — | `'zh'` \| `'en'` |
| `baseUrl` | `string` | — | 后端基础 URL |
| `api` | `string` | — | 获取树数据的 API 地址 |
| `nodes` | `FileNode[]` | — | 直接传入的节点数据 |

| Event | 参数 | 说明 |
|-------|------|------|
| `select` | `node: FileNode` | 选中节点时触发 |

| 暴露 | 说明 |
|------|------|
| `nodes` (ref) | 响应式节点数组 |

**导出类型**:
```ts
export interface FileNode {
  id: number
  name: string
  size: number
  type: 'folder' | 'file'
  suffix?: string
  children?: FileNode[]
  expanded?: boolean
}
```

---

### 4. Thumbnail — 文件缩略图/预览

根据文件扩展名自动识别类型并渲染对应的预览。

```vue
<Thumbnail
  :src="fileUrl"
  name="report.pdf"
  :fileId="123"
  baseUrl="https://api.example.com"
  fit="cover"
  :showName="true"
/>
```

| Prop | 类型 | 默认值 | 说明 |
|------|------|--------|------|
| `src` | `string` | — | 文件 URL 或 data URL |
| `name` | `string` | — | 文件名（用于类型判断和图标） |
| `fileId` | `number` | — | 后端文件 ID，拼接 `/thumbnail/{id}` |
| `baseUrl` | `string` | — | 后端基础 URL |
| `fit` | `'cover'` \| `'contain'` | `'cover'` | 图片缩放模式 |
| `showName` | `boolean` | — | 是否在图标下显示文件名 |

**自动识别类型**: 图片(jpg/png/gif/...)、视频(mp4/webm/...)、音频(mp3/wav/...)、PDF(通过 `/thumbnail/{id}` 获取缩略图)、其他(emoji 图标)

---

### 5. Markdown — Markdown 渲染器

基于 markdown-it + KaTeX 的 Markdown 渲染组件。

```vue
<Markdown :markdown="mdContent" />
```

| Prop | 类型 | 说明 |
|------|------|------|
| `markdown` | `string` | Markdown 原始文本 |

| 暴露 | 说明 |
|------|------|
| `refresh()` | 手动重新渲染 |

**特性**: 支持 GitHub Flavored Markdown、LaTeX 数学公式（KaTeX）、代码高亮、表格、YouTube/Twitter/Spotify/Apple Music 嵌入

---

### 6. Crud — 通用 CRUD 入口

泛型组件 `<Data>`，组合 SearchField + TabContainer + TablePaged，提供搜索、列表、查看/编辑一体化的 CRUD 入口。

```vue
<Crud
  lang="zh"
  caption="用户管理"
  api="/api/admin/users"
  :fields="userFields"
  :hpostdata="handlePostData"
  :component="UserDetailComponent"
  :selected="selectedUsers"
  :data__title="(u: User) => u.name"
  :empty__data="() => ({ id: 0, name: '' })"
  :data__id="(u: User) => u.id"
  :data__desc="(u: User) => u.name"
/>
```

| Prop | 类型 | 说明 |
|------|------|------|
| `lang` | `string` | 语言 |
| `caption` | `string` | 标题 |
| `api` | `string` | 数据 API 地址 |
| `fields` | `TableField[]` | 表格字段定义 |
| `hpostdata` | `Function` | 处理 postdata 的钩子 |
| `component` | `Component` | 编辑/查看用的详情组件 |
| `selected` | `Data[]` | 多选数组 |
| `data__title` | `(item: Data) => string` | 提取标题 |
| `empty__data` | `() => Data` | 创建空实例 |
| `data__id` | `(item: Data) => any` | 提取 ID |
| `data__desc` | `(item: Data) => string` | 提取描述文本 |

---

### 7. TabContainer — 标签页容器

多标签页管理组件，支持动态添加/关闭标签。

| Prop | 类型 | 默认值 | 说明 |
|------|------|--------|------|
| `defaultTabType` | `string` | `'dashboard'` | 默认标签类型 |
| `showAddBtn` | `boolean` | `false` | 是否显示新建按钮 |

| Event | 说明 |
|-------|------|
| `onClickCreate` | 点击新建按钮 |

| 暴露方法 | 说明 |
|----------|------|
| `createTab(item: TabItem)` | 创建新标签 |
| `activateTab(id)` | 激活指定标签 |
| `closeTab(id)` | 关闭指定标签 |

---

### 8. TablePaged — 分页数据表格

支持排序、多选、骨架屏、分页的数据表格。

| Prop | 类型 | 说明 |
|------|------|------|
| `lang` | `string` | 语言 |
| `fields` | `TableField[]` | 字段定义 `{ key, width?, sortable?, text? }` |
| `api` | `string` | 数据 API 地址 |
| `hpostdata` | `Function` | postdata 处理钩子 |
| `selected` | `Data[]` | 多选数组 |
| `onRowClick` | `(item: Data) => void` | 行点击回调 |

---

### 9. SearchField — 搜索输入框

泛型搜索组件，输入时异步查询后端并显示下拉选项。

| Prop | 类型 | 说明 |
|------|------|------|
| `api` | `string` | 搜索 API 地址 |
| `item__key` | `(item: Data) => string` | 选项 key |
| `item__text` | `(item: Data) => string` | 选项显示文本 |
| `onselect` | `(item: Data) => void` | 选中回调 |

---

## 二、网络工具

### HTTP 请求

```ts
import { post, get, upload, loader } from '@lchenmay/jcs-common'
```

#### `post(url, data)` / `get(url, data)`
通用 JSON POST/GET 请求，120s 超时，自动注入 session、Clerk token。

```ts
const rep = await post('/api/admin/users', { act: 'list' })
if (rep?.Er === 'OK') { /* 成功 */ }
```

#### `upload(suc, fail)(file, dst, desc)`
文件上传函数，使用 FileReader + fetch 以 `application/octet-stream` 上传。

```ts
upload(
  (rep) => console.log('uploaded', rep),
  (err) => console.error('failed', err)
)(file, '/api/public/upload', '账单文件')
```

#### `loader(url, postdata, h, ex)`
通用数据加载器，自动注入 session，检查 `rep.Er == "OK"`。

```ts
loader('/api/admin/listDir', { folderId: 0 },
  (rep) => { items.value = rep.rep.items },
  (err) => console.error(err)
)
```

> **重要**: 后端 API 必须返回包含顶层 `"Er":"OK"` 的 JSON，否则 `loader` 会走失败分支。

---

## 三、文件工具函数

```ts
import { formatSize, fileIcon, isImage } from '@lchenmay/jcs-common'
```

| 函数 | 签名 | 说明 |
|------|------|------|
| `formatSize` | `(bytes: number) => string` | 字节 → 可读字符串 (`1.5 MB`) |
| `fileIcon` | `(name: string) => string` | 文件名 → emoji 图标 |
| `isImage` | `(name: string) => boolean` | 判断是否为图片类型 |

**`fileIcon` 映射表**:

| 类型 | 图标 | 扩展名 |
|------|------|--------|
| 图片 | 🖼 | jpg, jpeg, png, gif, bmp, svg, webp, ico, heic, heif |
| PDF | 📕 | pdf |
| Word | 📘 | doc, docx |
| Excel | 📗 | xls, xlsx, csv |
| PPT | 📙 | ppt, pptx |
| 压缩包 | 📦 | zip, rar, 7z, tar, gz |
| 音频 | 🎵 | mp3, wav, ogg, flac, aac, m4a |
| 视频 | 🎬 | mp4, webm, ogg, mov, avi, mkv |
| 文本 | 📄 | txt, md, json, xml, yml, yaml, log |
| 网页 | 🌐 | html, htm |
| 前端源码 | 💻 | js, ts, jsx, tsx, vue, svelte |
| 后端源码 | ⚙️ | py, rb, go, rs, java, c, cpp, cs, fs, swift, kt |
| 其他 | 📎 | 所有未匹配类型 |

---

## 四、二进制序列化 (bin)

```ts
import { BytesBuilder, int32__bin, bin__int32, str__bin, bin__str, ... } from '@lchenmay/jcs-common'
```

用于 F# ↔ TypeScript 跨语言二进制协议。

**核心类型**:
```ts
type BinIndexed = { bin: ArrayBuffer; index: number }
```

**基础类型序列化**:

| 编码 | 解码 | 类型 |
|------|------|------|
| `int32__bin(bb)(v)` | `bin__int32(bi)` | `number` (int32) |
| `int64__bin(bb)(v)` | `bin__int64(bi)` | `number` (int64) |
| `float__bin(bb)(v)` | `bin__float(bi)` | `number` (float64) |
| `bool__bin(bb)(v)` | `bin__bool(bi)` | `boolean` |
| `str__bin(bb)(s)` | `bin__str(bi)` | `string` |
| `DateTime__bin(bb)(d)` | `bin__DateTime(bi)` | `Date` |
| `Json__bin(bb)(o)` | `bin__Json(bi)` | JSON string |

**泛型序列化**:
```ts
// Option
option__bin(item__bin)(bb)(value)
bin__option(bin__item)(bi)

// Array
array__bin(item__bin)(bb)(arr)
bin__array(bin__item)(bi)

// List (同 Array)
List__bin(item__bin)(bb)(arr)
bin__List(bin__item)(bi)

// Dictionary
dict__bin(k__bin)(v__bin)(bb)(dict)
bin__dict(bin__k)(bin__v)(bi)
```

**BytesBuilder**:
```ts
const bb = new BytesBuilder()
int32__bin(bb)(42)
str__bin(bb)("hello")
const bytes: ArrayBuffer = bb.bytes()
const b64: string = bb.base64()
```

---

## 五、认证 (Auth)

```ts
import { SignOut, biz__LoginOptions, LoginOption__RT, host__DiscordRedirectURL } from '@lchenmay/jcs-common'
```

| 函数 | 说明 |
|------|------|
| `SignOut()` | 清除 localStorage 并刷新页面 |
| `biz__LoginOptions(biz?)` | 从 URL 参数提取登录选项，默认 `"DISCORD"` |
| `LoginOption__RT(options, postFn, notifySuc)` | 发送登录请求并存储 session |
| `host__DiscordRedirectURL(host?)` | 生成 Discord OAuth 跳转 URL |

---

## 六、WebSocket

```ts
import { createWebSocket_base, disconnect, trySend, trySendx } from '@lchenmay/jcs-common'
```

| 函数 | 说明 |
|------|------|
| `createWebSocket_base(wsbinHandler, wsjsonHandler)(wsurl)(saveKey?)` | 创建 WebSocket 连接 |
| `disconnect(ws)` | 断开连接 |
| `trySend(wsCtx)(e)(msg)` | 发送消息 `{ e, val: msg }` |
| `trySendx(e)(msg)` | 从 `globalThis.runtime.wsctx` 发送 |

---

## 七、Markdown 解析函数

```ts
import { markdown__html, markdown__html_, html__toc } from '@lchenmay/jcs-common'
```

| 函数 | 说明 |
|------|------|
| `markdown__html(str)` | 自定义 Markdown → HTML（支持 LaTeX、图片、音频、链接） |
| `markdown__html_(str)` | 备选 Markdown 解析器（compile_md） |
| `html__toc(html)` | 从 HTML 提取目录 (Table of Contents) |

---

## 八、通用工具

```ts
import { buildHost, getLocalStorage, setLocalStorage, checkDomain, sleep, url__Params, decode, timestamp__str, url__param, __s, s__limitLength, amt__2digitDollar } from '@lchenmay/jcs-common'
```

| 函数 | 说明 |
|------|------|
| `buildHost()` | 构建运行时 host 配置（api, wsurl, discord 等） |
| `getLocalStorage(key, defaultv?)` | 读 localStorage（JSON 解析） |
| `setLocalStorage(key, value)` | 写 localStorage（JSON 序列化） |
| `checkDomain(domain)` | 检查当前域名是否匹配 |
| `sleep(ms)` | Promise 延迟 |
| `url__Params(url)` | URL → 参数对象 |
| `decode(str)` | 解码转义字符 `\n` `\r` `\\` |
| `timestamp__str(ts)` | 时间戳 → `YYYY/M/D H:M` |
| `url__param(name)(url)` | 从 URL 提取指定参数 |
| `__s(str)` | 解析多语言 JSON 字符串，取当前语言的值 |
| `s__limitLength(n)(s)` | 字符串截断 `...` |
| `amt__2digitDollar(n)` | 数字 → `$1,234.56` 格式 |

---

## 九、jlib 工厂函数

```ts
import { createJlib } from '@lchenmay/jcs-common'
```

创建项目级工具聚合器，将 vue、ws、notify、route、panel、mor 等模块聚合为一个对象。

```ts
const jlib = createJlib(mor, {
  ws, fetchs: { post, get },
  notify, route, panel, host,
  runtime: { setRT, getRT },
})
// jlib.vue, jlib.post, jlib.ws, jlib.notify, ...
```

---

## 十、路由工厂

```ts
import { createProjectRouter } from '@lchenmay/jcs-common'
```

创建 Vue Router 实例，支持 `/zh/` `/en/` 语言前缀自动识别和路由跳转。

```ts
const { router, navigate, incomingRoute } = createProjectRouter(routes, memoryHistory?)
```

---

## 十一、通知模块 (notify)

```ts
import { add, aSuc, aFail, aEx, msg_add, init } from '@lchenmay/jcs-common'
```

| 函数 | 说明 |
|------|------|
| `add(msg)` | 添加普通通知 |
| `aSuc(msg, exp?)` | 添加成功通知（绿色，默认 400ms） |
| `aFail(msg, exp?)` | 添加失败通知（红色，默认 1500ms） |
| `aEx(msg, exp?)` | 添加异常通知（橙色，默认 1500ms） |
| `msg_add(item: NotifyItem)` | 添加自定义通知项 |
| `init()` | 初始化通知轮询（每 500ms 清除过期消息） |

---

## 十二、项目中的实际使用示例

### WYI 项目使用 FileUploader

```vue
<script setup lang="ts">
import { FileUploader, formatSize } from '@lchenmay/jcs-common'

const onFileUploaded = (item: any) => {
  console.log('上传成功:', item.file.name, item.fileId)
}
</script>

<template>
  <FileUploader
    lang="zh"
    uploadUrl="/api/public/upload"
    accept="image/*,.pdf,.doc,.docx"
    @uploaded="onFileUploaded"
  />
</template>
```

### Aiarwa 项目使用 FileSysTree

```vue
<script setup lang="ts">
import { FileSysTree, type FileNode } from '@lchenmay/jcs-common'

const onSelect = (node: FileNode) => {
  if (node.type === 'file') {
    // 打开文件预览
  } else {
    // 展开文件夹
  }
}
</script>

<template>
  <FileSysTree
    lang="zh"
    api="/api/admin/listDir"
    @select="onSelect"
  />
</template>
```

### 使用 loader 加载数据

```ts
import { loader } from '@lchenmay/jcs-common'

const loadItems = () => {
  loader('/api/admin/listDir', { folderId: 0 },
    (rep) => {
      items.value = rep.rep.items
    },
    (err) => {
      console.error('加载失败:', err)
    }
  )
}
```

---

## 升级备忘录

- **当前版本**: `1.0.139`
- **升级步骤**: 改 `package.json` 版本号 → 删 `bun.lockb` → `bun install`
- **⚠ 重要**: 必须先删除 `bun.lockb`，否则可能安装不到最新版本
- 受影响的锁文件: `c:\Dev\WYI\vscode\bun.lockb`, `c:\Dev\Aiarwa\vscode\bun.lockb`
