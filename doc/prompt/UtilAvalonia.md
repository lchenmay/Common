# UtilAvalonia 组件库文档

## 概述

`UtilAvalonia` 是一个基于 Avalonia UI 框架的 F# 组件库，提供了一系列可复用的 UI 组件和工具函数。本文档涵盖了库中的所有公开组件、类型和函数。

## 命名空间与模块

```
UtilAvalonia.UiUtil
UtilAvalonia.Markdown
UtilAvalonia.Comp.FileSysTree
UtilAvalonia.Comp.FileSysTree.Logics
UtilAvalonia.Comp.FileSysTree.Layout
UtilAvalonia.Comp.LayoutGrid
UtilAvalonia.Comp.MainStatus
UtilAvalonia.Comp.MarkdownExpandable
UtilAvalonia.Comp.TabContainer
UtilAvalonia.Comp.TextConsole
```

---

## 1. UiUtil 模块 (`UtilAvalonia.UiUtil`)

### 类型：`BuildInColor`

预定义颜色枚举，用于 UI 元素的颜色设置。

```fsharp
type BuildInColor =
    | DefaultForeground = 0    // #E6E6A3 - 浅黄色
    | DefaultBackground = 1    // #0D2B1F - 深绿色
    | Green = 2                // #4ECB71
    | Red = 3                  // #E66A5C
    | Yellow = 4               // #F5D033
    | Cyan = 5                 // #45E3E3
    | White = 6                // #FFFFFF
    | ToolbarBackground = 101  // #2D2D30 - 深灰色
    | StatusPanelBackground = 201  // #2D2D30
    | StatusPanelForeground = 202  // #E6E6E3
    | TabBackground = 301      // #1E1E1E
```

### 全局常量

| 名称 | 类型 | 描述 |
|------|------|------|
| `defaultFontFamily` | `FontFamily` | 默认字体族："Fira Code, 'JetBrains Mono', 'Cascadia Code', 'Consolas', monospace" |
| `defaultFontSize` | `float` | 默认字体大小：14.0 |
| `defaultFontSizeSmall` | `float` | 小号字体大小：12.0 |
| `defaultCheckboxSize` | `float` | 默认复选框大小：12.0 |

### 函数

#### `color__Brush`
```fsharp
color__Brush: ccolor: BuildInColor -> SolidColorBrush
```
将 `BuildInColor` 枚举转换为对应的实心画刷。

#### `txt__TextBlock`
```fsharp
txt__TextBlock: text: string -> TextBlock
```
创建带有默认字体、字体大小和前景色的文本块。

#### `txtFontSize__TextBlock`
```fsharp
txtFontSize__TextBlock: fontsize: float -> text: string -> TextBlock
```
创建指定字体大小的文本块。

#### `txt__Button`
```fsharp
txt__Button: text: string -> Button
```
创建宽度为 60.0 的按钮。

#### `control__withBorder`
```fsharp
control__withBorder: c: Control -> Border
```
为控件添加带默认背景色和圆角（4.0）的边框。

#### `copyToClipboard`
```fsharp
copyToClipboard: output: (string -> unit) -> this: Control -> text: string -> unit
```
将文本复制到系统剪贴板，并通过 `output` 回调通知结果。

---

## 2. Markdown 模块 (`UtilAvalonia.Markdown`)

### 类型：`MarkdownRenderOptions`

```fsharp
type MarkdownRenderOptions = {
    FontSize: float           // 默认字体大小
    CodeFontSize: float       // 代码字体大小
    LinkColor: Color          // 链接颜色
    CodeBackground: Color     // 代码块背景色
}
```

### 默认配置

```fsharp
let defaultOptions = {
    FontSize = 12.0
    CodeFontSize = 11.0
    LinkColor = Color.Parse("#569CD6")
    CodeBackground = Color.Parse("#1E1E1E")
}
```

### 函数

#### `renderMarkdown`
```fsharp
renderMarkdown: text: string -> options: MarkdownRenderOptions -> StackPanel
```
将 Markdown 文本渲染为 Avalonia UI 控件树。支持以下语法：
- **标题**：`#`、`##`、`###`
- **列表**：`-`、`*`
- **引用**：`>`
- **代码块**：` ``` `
- **行内代码**：`` `code` ``
- **粗体**：`**text**`

#### `getPreviewText`
```fsharp
getPreviewText: text: string -> maxLines: int -> string
```
截取文本的前 `maxLines` 行作为预览，如果文本超过指定行数则附加 `"..."`。

---

## 3. 文件系统树组件 (`UtilAvalonia.Comp.FileSysTree`)

### `FileTreeView` 类

文件树视图组件，包含复选框选择功能和已选文件列表。

#### 构造函数

```fsharp
FileTreeView(
    getMainWindow: unit -> Window,          // 获取主窗口的函数
    onRootPathChanged: string -> unit,      // 根路径变更回调
    ?log: string -> unit,                   // 日志输出函数
    ?checkboxSize: float                    // 复选框大小，默认12.0
)
```

#### 属性

| 名称 | 类型 | 描述 |
|------|------|------|
| `RootNode` | `TreeViewItem` | 树视图的根节点 |
| `SelectedPaths` | `ObservableCollection<string>` | 已选路径集合 |

#### 方法

```fsharp
/// 添加根节点（用于开发项目和文档项目）
member AddRootNode: header: string -> tag: obj -> TreeViewItem

/// 添加带右键菜单的根节点
member AddRootNode: header: string -> tag: obj -> contextMenu: ContextMenu -> TreeViewItem

/// 设置根目录路径
member SetRootPath: path: string -> unit

/// 添加自定义右键菜单项到文件服务节点
member AddMenuItem: header: string -> onClick: (unit -> unit) -> unit

/// 刷新树（重新加载）
member Refresh: unit -> unit
```

#### 事件

```fsharp
/// 已选路径变更事件
member SelectedPathsChanged: IEvent<NotifyCollectionChangedEventArgs>
```

---

## 4. 文件系统树逻辑模块 (`UtilAvalonia.Comp.FileSysTree.Logics`)

### 内部辅助函数

#### `getSortedDirectories`
```fsharp
getSortedDirectories: dirPath: string -> string[]
```
获取目录下所有子目录名（已排序）。

#### `getSortedFiles`
```fsharp
getSortedFiles: dirPath: string -> string[]
```
获取目录下所有文件名（已排序）。

#### `createTreeNode`
```fsharp
createTreeNode: 
    header: string -> 
    path: string -> 
    isDirectory: bool -> 
    checkboxSize: float -> 
    TreeViewItem * CheckBox option
```
创建带复选框的树节点。返回节点和可选的复选框控件。

### 递归加载函数

#### `loadDirectoryWithCheckbox`
```fsharp
loadDirectoryWithCheckbox:
    node: TreeViewItem ->
    dirPath: string ->
    log: (string -> unit) ->
    checkboxSize: float ->
    selectedPaths: ObservableCollection<string> ->
    unit
```
递归加载目录内容到树节点，带有复选框同步到 `selectedPaths` 集合。

#### `loadDirectory`
```fsharp
loadDirectory:
    node: TreeViewItem ->
    dirPath: string ->
    log: (string -> unit) ->
    checkboxSize: float ->
    unit
```
递归加载目录内容到树节点（不带复选框同步）。

---

## 5. 文件系统树布局模块 (`UtilAvalonia.Comp.FileSysTree.Layout`)

### `createFileTreeLayout`
```fsharp
createFileTreeLayout:
    treeView: TreeView ->
    selectedPaths: ObservableCollection<string> ->
    clearSelectedPaths: (unit -> unit) ->
    log: (string -> unit) ->
    Grid
```
创建文件树视图的完整布局，包含：
- 顶部：已选路径列表 + 清空按钮
- 底部：树视图（自动填充剩余空间）

---

## 6. 布局网格组件 (`UtilAvalonia.Comp.LayoutGrid`)

### `LayoutGrid` 类

基于 `Grid` 的布局容器，支持按比例定义行和列。

#### 构造函数

```fsharp
LayoutGrid(rows: float[], cols: float[])
```
- `rows`: 行高比例数组
- `cols`: 列宽比例数组

#### 方法

```fsharp
/// 添加子控件到指定位置
member AddChild: (row * col): int * int -> child: Control -> unit

/// 添加跨越行列的子控件
member AddChildSpan: 
    (row * col): int * int -> 
    (rowSpan * colSpan): int * int -> 
    child: Control -> 
    unit

/// 清除所有子控件
member Clear: unit -> unit
```

### 使用示例

```fsharp
let grid = LayoutGrid([|1.0; 2.0|], [|1.0; 1.0|])
grid.AddChild (0, 0) textBlock1
grid.AddChildSpan (0, 0) (1, 2) textBlock2  // 跨越两列
```

---

## 7. 主状态栏组件 (`UtilAvalonia.Comp.MainStatus`)

### `MainStatus` 类

底部状态栏组件，包含状态文本、进度条和时钟显示。

#### 构造函数

```fsharp
MainStatus(?log: string -> unit)
```

#### 属性

| 名称 | 类型 | 描述 |
|------|------|------|
| `StatusBar` | `Border` | 状态栏控件实例 |

#### 方法

```fsharp
/// 设置状态文本
member SetStatus: text: string -> unit

/// 设置进度
member SetProgress: value: float -> max: float -> unit

/// 设置不确定进度（进度条动画）
member SetProgressIndeterminate: unit -> unit

/// 清除进度
member ClearProgress: unit -> unit

/// 停止时钟定时器
member Stop: unit -> unit
```

#### 事件

```fsharp
member StatusChanged: IEvent<string>
```

#### 接口实现

`IDisposable` - 调用 `Dispose()` 会停止时钟定时器。

---

## 8. 可折叠 Markdown 组件 (`UtilAvalonia.Comp.MarkdownExpandable`)

### `MarkdownExpandableOptions` 类型

```fsharp
type MarkdownExpandableOptions = {
    BackgroundColor: Color           // 背景色
    CornerRadius: CornerRadius       // 圆角
    Margin: Thickness                // 外边距
    Padding: Thickness               // 内边距
    ButtonWidth: float               // 展开/折叠按钮宽度
    ButtonHeight: float              // 展开/折叠按钮高度
    ButtonMargin: Thickness          // 按钮外边距
    PreviewLines: int                // 预览行数
    MarkdownOptions: MarkdownRenderOptions  // Markdown 渲染选项
}
```

### `MarkdownExpandable` 类

可折叠的 Markdown 内容显示控件。

#### 构造函数

```fsharp
MarkdownExpandable(
    content: string,                   // Markdown 内容
    isExpanded: bool,                  // 是否展开
    ?options: MarkdownExpandableOptions  // 配置选项
)
```

#### 属性

| 名称 | 类型 | 描述 |
|------|------|------|
| `IsExpanded` | `bool` | 获取或设置展开状态 |
| `Content` | `string` | 获取完整内容 |

#### 方法

```fsharp
/// 设置新内容
member SetContent: newContent: string -> unit

/// 手动切换展开/折叠
member ToggleExpand: unit -> unit
```

---

## 9. 标签容器组件 (`UtilAvalonia.Comp.TabContainer`)

### `TabContainer` 类

标签页管理容器。

#### 构造函数

```fsharp
TabContainer()
```

#### 方法

```fsharp
/// 添加新标签页
member AddTab: 
    header: string ->         // 标签标题
    isClosable: bool ->       // 是否可关闭
    content: Control ->       // 内容控件
    TabItem                   // 返回标签页项

/// 打开或激活标签页（如果已存在则切换）
member OpenOrActivateTab: 
    header: string ->
    isClosable: bool ->
    content: Control ->
    unit
```

---

## 10. 文本控制台组件 (`UtilAvalonia.Comp.TextConsole`)

### `ColoredSegment` 类型

```fsharp
type ColoredSegment = {
    Text: string
    Color: BuildInColor
}
```

### `TextConsole` 类

文本控制台组件，支持颜色输出、清空、复制、保存功能。

#### 构造函数

```fsharp
TextConsole(?log: string -> unit)
```

#### 属性

| 名称 | 类型 | 描述 |
|------|------|------|
| `BufferSize` | `int` | 获取或设置缓冲区大小（默认1000） |

#### 方法

```fsharp
/// 追加文本（不换行）
member Write: text: string -> unit

/// 写入一行文本
member WriteLine: text: string -> unit

/// 写入带颜色的文本（不换行）
member WriteColor: ccolor: BuildInColor -> text: string -> unit

/// 写入带颜色的新行
member NewLineColor: ccolor: BuildInColor -> text: string -> unit

/// 写入带颜色段的文本（不换行）
member WriteSegments: segments: seq<ColoredSegment> -> unit

/// 写入带颜色段的新行
member WriteLineSegments: segments: seq<ColoredSegment> -> unit

/// 清空控制台
member Clear: unit -> unit

/// 刷新显示
member Flush: unit -> unit

/// 停止刷新定时器
member Stop: unit -> unit
```

#### 接口实现

`IDisposable` - 调用 `Dispose()` 会停止刷新定时器。

---

## 注意事项

1. **线程安全**：所有 UI 更新操作应在 UI 线程执行。
2. **文件系统树**：目录加载在后台线程执行，通过 `Dispatcher.UIThread.Post` 更新 UI。
3. **依赖项**：需要 Avalonia 12.0.3+ 和 `Util.fsproj` 项目引用。