# /Common/Util JSON 处理模块详细文档

## 概述

`Util.Json` 是 F# 语言实现的 JSON 处理模块，提供从字符串解析 JSON、构建 JSON 对象、序列化 JSON 为字符串以及查询/修改 JSON 结构等完整功能。该模块采用自定义的 `Json` 联合类型（Discriminated Union）表示 JSON 数据结构，不依赖第三方 JSON 库。

## 核心数据结构

### `Json` 类型

```fsharp
type Json = 
| Str of string       // JSON 字符串值
| Num of string       // JSON 数值（以字符串形式存储）
| True                // JSON true 值
| False               // JSON false 值
| NoBraket of (string * Json)[]  // 无花括号的对象（用于特殊场景）
| Braket of (string * Json)[]    // JSON 对象（花括号包围）
| Ary of Json[]       // JSON 数组
| Null                // JSON null 值
```

### `Token` 类型（解析中间步骤）

```fsharp
type Token = 
| StrQuoted of string   // 双引号引用的字符串
| StrGeneral of string  // 未引用的字符串（如 true、false、null、数字）
| Symbol of char        // 符号字符：{ } [ ] , :
| Undefined             // 未定义
```

### `empty` 值

```fsharp
let empty = Json.Braket [||]
```
表示空 JSON 对象 `{}`。

## 字符串转义处理

### `decode` 函数

将 JSON 字符串中的转义序列转换为实际字符。

**语法：** `decode(s: string) : string`

**支持的转义序列：**

| 转义序列 | 转换结果 |
|---------|---------|
| `\\`   | `\`    |
| `\n`   | 换行符  |
| `\r`   | 回车符  |
| `\"`   | `"`    |

**注意：** 函数首先调用 `unescape_unicode`（来自 `Util.Text` 模块）处理 Unicode 转义序列。

### `encode` 函数

将字符串中的特殊字符转换为 JSON 转义序列。

**语法：** `encode(s: string) : string`

**转义规则：**

| 原始字符 | 转义结果 |
|---------|---------|
| `\`    | `\\`   |
| 换行符  | `\n`   |
| 回车符  | `\r`   |
| `"`    | `\"`   |

## JSON 解析

### `str__tokens` 函数

将 JSON 字符串拆分为 Token 序列。

**语法：** `str__tokens(s: string) : Token[]`

**处理流程：**
1. 遍历字符串字符
2. 处理双引号内的字符串（支持转义字符）
3. 将符号字符 `{`、`}`、`[`、`]`、`:`、`,` 识别为 `Symbol` Token
4. 将未引用的字符串识别为 `StrGeneral` Token
5. 过滤空字符串 Token

### 递归解析函数

#### `parseBraket` 函数

**语法：** `parseBraket(index: int ref, tokens: Token[]) : Json`

解析 JSON 对象（花括号内容）。

**处理流程：**
- 查找 `:` 符号分隔键值对
- 键必须是 `StrQuoted` Token
- 值可以是：`StrQuoted`（字符串）、`StrGeneral`（true/false/null/数字）、`Symbol '['`（数组）、`Symbol '{'`（嵌套对象）
- 遇到 `}` 或 `]` 时结束

#### `parseArray` 函数

**语法：** `parseArray(index: int ref, tokens: Token[]) : Json`

解析 JSON 数组（中括号内容）。

**处理流程：**
- 处理嵌套数组（遇到 `[` 递归调用）
- 处理嵌套对象（遇到 `{` 调用 `parseBraket`）
- 处理字符串值（`StrQuoted`）
- 处理基本值（`StrGeneral` 转换为 true/false/null 或数字）
- 遇到 `]` 时结束

### `str__root` 函数

**语法：** `str__root(s: string) : Json`

从字符串解析 JSON 的入口函数。

**处理流程：**
1. 调用 `str__tokens` 生成 Token 序列
2. 根据第一个 Token 类型决定调用：
   - `Symbol '['` → 调用 `parseArray`
   - 其他情况 → 调用 `parseBraket`
3. 空字符串返回 `Json.Null`

## JSON 序列化

### `json__str` 函数

**语法：** `json__str(w: TextBlockWriter, json: Json) : unit`

将 `Json` 值写入 `TextBlockWriter`。

**输出格式：**

| Json 类型 | 输出格式 |
|----------|---------|
| `Str`    | `"content"`（自动转义） |
| `Num`    | 直接输出数字字符串 |
| `True`   | `true` |
| `False`  | `false` |
| `Null`   | `null` |
| `Braket` | `{"key1":"val1","key2":val2,...}` |
| `NoBraket` | `"key1":"val1","key2":val2,...` |
| `Ary`    | `[item1,item2,...]` |

**特性：** 自动处理缩进，在对象/数组的最后一个元素后删除多余的逗号。

### `json__strFinal` 函数

**语法：** `json__strFinal(json: Json) : string`

直接返回 JSON 字符串。

**注意：** 内部创建新的 `TextBlockWriter`，序列化完成后返回字符串。

## JSON 查询函数

### 基本查询

#### `tryFindByAtt`

**语法：** `tryFindByAtt(attName: string, json: Json) : (string * Json) option`

在 JSON 对象中查找指定名称的属性。

```fsharp
// 示例
match json |> tryFindByAtt "name" with
| Some (name, value) -> value
| None -> Json.Null
```

#### `tryFindByPath`

**语法：** `tryFindByPath(path: string[], json: Json) : (string * Json) option`

按路径链查找属性。

```fsharp
// 示例：查找 address.city
let result = json |> tryFindByPath [|"address"; "city"|]
```

### 属性值查询

#### `tryFindStrByAttWithDefault`

**语法：** `tryFindStrByAttWithDefault(dft: string, attName: string, json: Json) : string`

查找字符串属性值，带默认值。

#### `tryFindStrByAtt`

**语法：** `tryFindStrByAtt(attName: string, json: Json) : string`

查找字符串属性值，默认返回空字符串。

#### `tryFindNumByAtt`

**语法：** `tryFindNumByAtt(attName: string, json: Json) : string`

查找数值属性（返回字符串形式）。

#### `tryFindBoolByAtt`

**语法：** `tryFindBoolByAtt(attName: string, json: Json) : string`

查找布尔属性，返回 `"true"` 或 `"false"`。

### 带类型转换的查询

#### `tryFindInt32ByAttWithDefault`

**语法：** `tryFindInt32ByAttWithDefault(dft: int, attName: string, json: Json) : int`

查找整数属性。

#### `tryFindFloatByAttWithDefault`

**语法：** `tryFindFloatByAttWithDefault(dft: float, attName: string, json: Json) : float`

查找浮点数属性。

#### `tryFindBoolByAttWithDefault`

**语法：** `tryFindBoolByAttWithDefault(dft: bool, attName: string, json: Json) : bool`

查找布尔属性。

### 数组查询

#### `tryFindAryByAtt`

**语法：** `tryFindAryByAtt(attName: string, json: Json) : Json[]`

查找数组属性，未找到返回空数组。

#### `tryFindTrueByAtt` / `tryFindFalseByAtt`

**语法：**
```fsharp
tryFindTrueByAtt(attName: string, json: Json) : bool
tryFindFalseByAtt(attName: string, json: Json) : bool
```

分别检查属性是否为 `true` 或 `false`。

### 辅助查询函数

#### `json__tryFindByName`

**语法：** `json__tryFindByName(json: Json, name: string) : Json option`

在对象中按名称查找，返回 `Json` 值。

#### `name__valo`

**语法：** `name__valo(braket: Json, name: string) : Json option`

与 `json__tryFindByName` 功能类似。

#### `name__array`

**语法：** `name__array(braket: Json, name: string) : Json[]`

按名称查找数组属性。

#### `json__aryItems`

**语法：** `json__aryItems(attName: string, json: Json) : Json[] option`

返回指定数组属性的所有项目。

#### `json__arySomeItems`

**语法：** `json__arySomeItems(json__itemo: Json -> 'T option, attName: string, json: Json) : 'T[] option`

查找数组属性，并对每个项目应用转换函数，过滤掉转换结果为 None 的项目。

## JSON 修改函数

### `replaceAtt`

**语法：** `replaceAtt(attName: string, node: Json, src: Json) : Json`

替换对象中的指定属性值。如果源不是对象，则创建新对象。

### `tryAddBracket`

**语法：** `tryAddBracket<'T>(json: Json, attName: string, attValue: 'T) : Json option`

向对象添加一个属性。支持的类型：
- `string` → 转换为 `Json.Str`
- `int32` / `int64` / `float` → 转换为 `Json.Num`
- `bool` → 转换为 `Json.True` 或 `Json.False`
- 其他类型 → 忽略

**注意：** 仅当输入为 `Json.Braket` 时有效。

### `tryAddBrackets`

**语法：** `tryAddBrackets<'T>(json: Json, attKVs: (string * 'T)[]) : Json option`

批量添加多个属性。

## 辅助函数

### `kvp`

**语法：** `kvp(k: string, v: Json) : (string * Json)`

创建键值对（语法糖）。

### 类型检查函数

#### `json__braketo`

**语法：** `json__braketo(json: Json) : (string * Json)[] option`

检查是否为对象，返回其项目数组。

#### `json__aryo`

**语法：** `json__aryo(json: Json) : Json[] option`

检查是否为数组，返回其项目数组。

### `json__items`

**语法：** `json__items(json: Json) : Dictionary<string, string>`

将 JSON 对象转换为 `Dictionary`，键为属性名（小写），值为字符串表示。

**注意：** 仅处理基本类型（字符串、数字、布尔、null），忽略嵌套对象和数组。

### `jsonstr__items`

**语法：** `jsonstr__items(s: string) : Dictionary<string, string>`

组合函数：先解析 JSON 字符串，再转换为 Dictionary。

### `check_mandatory_fields`

**语法：** `check_mandatory_fields(fields: Dictionary<string, string>, keys: string[]) : string[]`

检查必填字段是否都存在，返回缺失字段列表（此函数可能位于 `Json.fs` 但代码被截断）。

## 使用示例

### 1. 解析 JSON

```fsharp
open Util.Json

// 解析 JSON 字符串
let jsonString = """
{
    "name": "Alice",
    "age": 30,
    "active": true,
    "address": {
        "city": "Beijing",
        "country": "China"
    },
    "hobbies": ["reading", "swimming"]
}
"""

let json = str__root jsonString
```

### 2. 访问属性

```fsharp
// 获取字符串属性
let name = json |> tryFindStrByAtt "name"
// name = "Alice"

// 获取数值属性
let age = json |> tryFindInt32ByAttWithDefault 0 "age"
// age = 30

// 获取布尔属性
let isActive = json |> tryFindBoolByAttWithDefault false "active"
// isActive = true

// 嵌套属性访问
let city = json |> tryFindByPath [|"address"; "city"|]
// Some("city", Json.Str "Beijing")

// 获取数组
let hobbies = json |> tryFindAryByAtt "hobbies"
// hobbies = [|Json.Str "reading"; Json.Str "swimming"|]
```

### 3. 构建 JSON

```fsharp
// 创建空对象
let obj = Json.Braket [||]

// 添加属性
let person = 
    obj 
    |> tryAddBracket "name" "Bob"
    |> tryAddBracket "age" 25
    |> tryAddBracket "active" true
    // person = Some(Json.Braket [|("name", Str "Bob"); ("age", Num "25"); ("active", True)|])

// 批量添加
let data = 
    obj 
    |> tryAddBrackets [|
        "key1", "value1"
        "key2", 100
        "key3", false
    |]
```

### 4. 序列化

```fsharp
let serialized = json__strFinal json
// 输出格式化的 JSON 字符串
```

### 5. 转换为 Dictionary

```fsharp
let dict = json__items json
// 可用于 SQLite 的 key-value 存储
let cityValue = dict.["city"]  // "Beijing"
```

## 性能考虑

- 该实现不依赖反射和第三方库，性能中等
- 解析使用递归下降算法，对于深度嵌套的 JSON 需要注意栈溢出风险
- 字符串操作使用 `StringBuilder` 和 `List<char>` 提高性能
- 对于大量 JSON 处理场景，建议考虑使用 `System.Text.Json` 或 `Newtonsoft.Json`

## 注意事项

1. **字符串转义：** JSON 字符串中的转义序列由 `decode`/`encode` 处理，但仅支持基本转义，完整 Unicode 转义由 `unescape_unicode` 处理
2. **数字类型：** 所有数字均以字符串形式存储，类型转换由查询函数完成
3. **大小写敏感：** `json__items` 转换为 Dictionary 时键被转为小写，但其他查询函数保持原始大小写
4. **可变性：** `tryAddBracket` 和 `tryAddBrackets` 返回新对象，不会修改原对象
5. **空值处理：** 未找到属性时返回默认值或空数组，不会抛出异常

## 依赖模块

- `Util.Text`：提供 `unescape_unicode` 函数和字符串处理工具
- `Util.Time`：时间相关功能
- `Util.CollectionModDict` 和 `Util.Collection`：集合操作工具
- `TextBlockWriter`：用于格式化的字符串构建器（定义在项目其他地方）