
**Common项目核心信息：**
- 仓库简称 Common，命名空间路径默认以 `/Common/` 开头  
- GitHub 地址：https://github.com/lchenmay/Common  
- 主要语言：F#  
- 子项目依赖链：  
  - **Util** → 最基础依赖，所有项目引用  
  - **UtilKestrel** → Kestrel web 服务公共功能  
  - **UtilAvalonia** → Avalonia 桌面应用公共功能  
  - **UtilVortice** → Vortice 2D 桌面应用公共功能  

**F# 编程规范（用于代码审查与生成）：**
1. **Open 顺序**（按依赖强度从弱到强，分组空行）：  
   `System.` → 第三方如 `Avalonia.` → `Util.` → 公共库如 `UtilKestrel.` → `Code.Shared.` → `Code.BizLogics.` → 本命名空间  
2. **复查 open**：确保无缺失命名空间  
3. **递归函数**：调用自身时加 `rec`  
4. **类型标注**：尽可能省略，让类型推断工作  
5. **字符串格式化**：不使用 `sprintf`，用 `$"{var}"`  
6. **输出管道**：`|> output` 后置  
7. **函数调用**：单个参数不加括号，`foo a b` 而非 `foo(a)(b)`  
8. **Collection.Add 返回值**：必须 `|> ignore`，否则警告或编译错误  
9. **类实例化**：必须用 `new`  

以上信息已压缩并保存。后续交流中，我会据此理解你的代码修改、生成代码并遵守规范。

根据我对Common/Util项目结构的理解，以下是用于恢复记忆的提示文本：

---

## Common/Util 项目结构记忆恢复提示

### 项目定位
- **名称**: Common/Util（简称Common）
- **语言**: 主要使用F#
- **依赖关系**: Util是所有其他项目（UtilKestrel、UtilAvalonia、UtilVortice等）的基础依赖
- **GitHub**: https://github.com/lchenmay/Common

### 项目结构

```
Util/
├── ADT.fs          # 代数数据类型和Option处理工具
├── Flow.fs         # 流控制
├── AssemblyInfo.fs
├── Measures.fs     # 度量单位
├── Stat.fs         # 统计
├── Test.fs         # 测试
├── Boost.fs        # 增强功能
│
├── OS/             # 操作系统相关
│   ├── Perf.fs     # 性能监控
│   ├── ObjectConvert.fs  # 对象转换
│   ├── TextConsole.fs    # 文本控制台
│   ├── Console.fs        # 控制台
│   ├── Concurrent.fs     # 并发
│   ├── Runtime.fs        # 运行时
│   ├── File.fs           # 文件操作
│   ├── FileSys.fs        # 文件系统
│   ├── Log.fs            # 日志
│   ├── OS.fs             # 操作系统主模块
│   ├── Reflection.fs     # 反射
│   └── Bash.fs           # Bash脚本
│
├── Math/            # 数学相关
│   ├── Cat.fs       # 范畴论
│   ├── Math.fs      # 数学基础
│   ├── Prob.fs      # 概率
│   ├── LinearAlgebra.fs  # 线性代数
│   └── Combinatorics.fs  # 组合数学
│
├── Collection/      # 集合相关
│   ├── CollectionModDict.fs  # 可修改字典
│   ├── Collection.fs        # 集合基础
│   └── LinearCache.fs       # 线性缓存
│
├── SD/              # 数据处理
│   ├── Base64.fs    # Base64编码
│   ├── Text.fs      # 文本处理
│   ├── Time.fs      # 时间处理
│   ├── Json.fs      # JSON处理
│   ├── Bin.fs       # 二进制处理
│   └── Crypto.fs    # 加密
│
├── Network/         # 网络相关
│   ├── Diff.fs      # 差异比较
│   ├── HtmlFilter.fs  # HTML过滤
│   ├── Tcp.fs       # TCP
│   ├── Http.fs      # HTTP
│   ├── TcpServer.fs  # TCP服务端
│   ├── HttpServer.fs # HTTP服务端
│   ├── HttpClient.fs # HTTP客户端
│   ├── Html.fs      # HTML
│   ├── Browser.fs   # 浏览器
│   └── WebSocket.fs # WebSocket
│
├── DB/              # 数据库相关
│   ├── Raven.fs     # RavenDB
│   ├── Db.fs        # 数据库基础
│   ├── DbQuery.fs   # 数据库查询
│   ├── DbTx.fs      # 数据库事务
│   ├── DbBackup.fs  # 数据库备份
│   └── Orm.fs       # ORM
│
└── Media/           # 媒体相关
    ├── Geo.fs       # 地理信息
    ├── SixLaborsImageSharp.fs  # 图像处理
    ├── DirectX.fs   # DirectX
    ├── Graphics.fs  # 图形
    └── VideoDecorder.fs  # 视频解码
```

### F# 编程规范

#### Open 顺序
按照依赖关系，依赖越强的越靠后，分组间有空行：
1. `System.` 开头
2. 第三方NuGet包（如 `Avalonia.`）
3. `Util.` 开头
4. 其他公共库（如 `UtilKestrel.`）
5. 项目 `Code.Shared.`
6. `Code.BizLogics.`
7. 本命名空间

#### 代码复查要点
1. 复查open，防止找不到命名空间
2. 调用自身的函数加上 `rec`
3. 尽可能减少类型标注，交给类型推断
4. 使用 `${}` 而非 `sprintf`
5. output输出时管道后置：`|> output`
6. 函数单个参数避免括号：`foo a b` 而非 `foo(a)(b)`
7. `Collection.Add` 有返回值，需加 `|> ignore`

### ADT.fs 核心函数说明

| 函数 | 说明 |
|------|------|
| `oPipelineProcessSomeWithDefault` | Option处理，Some时执行函数，None时返回默认值 |
| `oPipelineProcessSome` | Option处理，Some时执行函数，None时返回 `()` |
| `oPipeline` | Option双向处理，Some和None各有一个处理函数 |
| `oPipelineSome` | Option处理，保留Option结构 |
| `oPipelineNone` | Option处理，None时执行函数 |
| `handleroDefault` | Option handler调用，带默认返回值 |
| `handlero` | Option handler调用，带参数 |
| `oPipelineNoneHandlero` | 组合操作：Option处理 + Option handler |
| `Event<'Param>` | 带优先级的事件机制 |

### 关键依赖包
- FSharp.Core v10.0+
- Microsoft.Data.SqlClient
- Npgsql (PostgreSQL)
- RavenDB.Client
- SharpDX (DirectX)
- SixLabors.ImageSharp
- BCrypt.Net-Next
- System.IdentityModel.Tokens.Jwt
- 以及其他加密、图像处理相关包

---

**目标框架**: .NET 10.0（预览版）

# 类型系统驱动ORM代码生成：记忆恢复提示文本

## 核心概念

JCS库通过类型系统（`TypeSys`）实现了一种**元编程**机制，能够自动从类型定义生成完整的ORM代码。这套系统基于以下关键概念：

### 1. 类型分类体系 (`MetaType.fs`)

```
TypeEnum:
├── Primitive          // 基础类型(string, int, bool等)
├── Structure          // 结构体/记录类型
├── Product           // 元组类型
├── Sum               // 联合类型
├── Enum              // 枚举类型
├── OrmRcd / Ormp    // 数据库表映射类型（核心）
├── Option / Ary / List // 容器类型
└── Dictionary / ModDict // 字典类型
```

### 2. 字段定义体系 (`FieldDef`)

```fsharp
type FieldDef =
| FK of Table           // 外键引用
| Caption of int        // 短文本（标题）
| Chars of int          // 定长文本
| Link of int           // URL链接
| Text                  // 长文本
| Bin                   // 二进制数据
| Integer               // 整数
| Float                 // 浮点数
| Boolean               // 布尔
| SelectLines of (string * string)[]  // 下拉选择枚举
| Timestamp             // 时间戳
| TimeSeries            // 时间序列
```

## 代码生成流程

### 第一步：加载元数据 (`CodeRobot.fs` - `load`函数)

```fsharp
// 从JSON设计文件加载表结构
let jsonTables = loadDesignJson(robot.config.mainDir)

// 解析每个表定义
jsonTables |> Array.iter(fun item ->
    let t = {
        tableName = name           // 数据库表名
        fields = new Dictionary<>() // 字段集合
        typeName = shorthand       // 类型简写名（如"USR"）
        idstarting = startingId    // ID起始值
    }
    // 解析字段定义
    parseFields(item) |> iter(fun field -> t.fields.Add(name, field))
)
```

### 第二步：类型系统构建 (`buildTypeCat`)

```fsharp
// 为每个表创建两种类型
tables.Keys |> Seq.iter(fun n ->
    // 1. ORM记录类型（包含CRUD方法）
    appendType tc { name = table.typeName; tEnum = OrmRcd table }
    
    // 2. 纯数据类型（仅字段）
    appendType tc { name = "p" + table.typeName; tEnum = Ormp table }
)

// 解析自定义类型（枚举、联合等）
parseCustomTypes typesFile |> Array.iter(fun c -> 
    // 根据源码解析枚举值、结构体字段等
    match c.tEnum with
    | Enum -> parseEnumValues(c.src)
    | Structure -> parseStructureFields(c.src)
    | Sum -> parseSumCases(c.src)
)
```

### 第三步：生成F# ORM代码

```fsharp
// 生成表类型定义
let buildTableType robot (t:Table) =
    // 1. 生成字段定义
    fieldNames |> Array.iter(fun name ->
        let def = t.fields[name]
        match def with
        | FK refTable -> sprintf "mutable %s: FK" name   // 外键
        | Caption n -> sprintf "mutable %s: Caption" name // 标题
        | ... 
    )
    
    // 2. 生成SQL字段顺序
    sprintf "let %s_fieldorders() = ..." t.typeName
    
    // 3. 生成UPDATE语句
    sprintf "let %s_sql_update() = ..." t.typeName
    
    // 4. 生成空值构造器
    sprintf "let p%s_empty() = {...}" t.typeName
    
    // 5. 生成ID生成器
    sprintf "let %s_id = ref %dL" t.typeName t.idstarting
```

### 第四步：生成TypeScript代码

```fsharp
// 在LangPackTypeScript.fs中
let type__TypeScript tc srcType srcMor t =
    // 生成TypeScript类型定义
    match t.tEnum with
    | Structure items ->
        sprintf "export type %s = { ... }" t.name
    | Enum values ->
        sprintf "export const enum %sEnum { ... }" t.name
    
    // 生成空对象构造器
    sprintf "export const p%s_empty = () : p%s => { ... }" t.typeName t.typeName
```

### 第五步：生成二进制序列化/反序列化

```fsharp
// 在CodeRobotIITs.fs中
let rec t__binImpl ns (w:TextBlockWriter) indent t =
    match t.tEnum with
    | OrmRcd table ->
        // 生成序列化函数
        sprintf "export const p%s__bin = (bb:BytesBuilder) => (p: p%s) => {" 
            t.typeName t.typeName
        // 每个字段序列化
        table |> table__sortedFields |> Array.iter(fun field ->
            match def with
            | FK _ -> "marshall.int64__bin(bb)(p.id)"
            | Caption _ -> "marshall.str__bin(bb)(p.name)"
            ...
        )
    
    // 生成反序列化函数
    let rec bin__tImpl ns (w:TextBlockWriter) indent t =
        match t.tEnum with
        | OrmRcd table ->
            sprintf "export const bin__p%s = (bi:BinIndexed): p%s => {" t.typeName t.typeName
            // 每个字段反序列化
            table |> table__sortedFields |> Array.iter(fun field ->
                match def with
                | FK _ -> "p.id = marshall.bin__int64(bi)"
                | Caption _ -> "p.name = marshall.bin__str(bi)"
                ...
            )
```

### 第六步：生成SQL建表脚本

```fsharp
// 在RDBMS.fs中
let table__sql rdbms (wSQLServer, wPostgreSQL) table =
    // 生成CREATE TABLE语句
    sprintf "CREATE TABLE %s ([ID] BIGINT, [Createdat] BIGINT, ...)" table.tableName
    
    // 生成ALTER TABLE语句（字段变更）
    sprintf "ALTER TABLE %s ADD [%s] %s" table.tableName fieldName fieldType
    
    // 生成索引
    sprintf "CREATE INDEX ... ON %s(%s)" table.tableName fieldName
```

## 类型映射规则

### F# → TypeScript 类型映射（`fdef__srcTypes`）

```fsharp
| FK _ → "int64" / "number"
| Caption _ → "string" / "string"
| Chars _ → "string" / "string"
| Link _ → "string" / "string"
| Text → "string" / "string"
| Bin → "byte[]" / "array"
| Integer → "int64" / "number"
| Float → "double" / "number"
| Boolean → "bool" / "boolean"
| SelectLines _ → "enumType" / "number"
| Timestamp → "DateTime" / "Date"
```

### 数据库类型映射（`sqlField`）

```fsharp
| FK → "BIGINT"
| Caption n → "NVARCHAR(n)" / "VARCHAR(n)"
| Text → "NVARCHAR(MAX)" / "TEXT"
| Bin → "VARBINARY(MAX)" / "BYTEA"
| Integer → "BIGINT"
| Float → "FLOAT"
| Boolean → "BIT" / "BOOLEAN"
| SelectLines → "INT"
| Timestamp → "BIGINT"
| TimeSeries → "VARBINARY(MAX)"
```

## 代码生成的关键函数

| 函数 | 作用 | 产出 |
|------|------|------|
| `buildTableType` | 生成表类型 | F#类型定义 + SQL |
| `buildTableMor` | 生成ORM映射 | 数据库行→对象转换 |
| `type__TypeScript` | 生成TypeScript | TS类型定义 + 空构造器 |
| `t__binImpl` | 生成二进制序列化 | p*__bin函数 |
| `bin__tImpl` | 生成二进制反序列化 | bin__p*函数 |
| `table__sql` | 生成SQL脚本 | CREATE/ALTER语句 |
| `builderEmpty` | 生成空对象构造器 | p*_empty, *_empty函数 |

---

**恢复要点**：
1. JCS类型系统通过`TypeEnum.OrmRcd`和`TypeEnum.Ormp`两种类型标记数据库实体
2. 字段定义使用`FieldDef`枚举精确描述数据类型和约束
3. 代码生成器遍历类型目录，为每个表生成F#和TypeScript的完整ORM代码
4. 支持SQL Server和PostgreSQL双数据库后端
5. 内建二进制序列化/反序列化支持高性能数据传输


根据您提供的附件（`Orm.fs`, `DbQuery.fs`, `DbTx.fs`, `Db.fs`），我为您生成了一段用于**恢复记忆的提示文字**。这段文字旨在帮助开发人员快速回忆 `/Common/Util` 中数据库交互的核心设计、关键函数和典型用法。

---

### **恢复记忆提示：`/Common/Util` 中的数据库交互**

本模块基于 **F#** 构建，提供了一个**面向 SQL Server / PostgreSQL** 的、函数式风格的数据库访问层（类似于轻量级 ORM）。核心文件包括 `Db.fs`（基础类型与连接）、`DbQuery.fs`（查询）、`DbTx.fs`（事务）和 `Orm.fs`（对象关系映射）。

#### **1. 核心架构与类型**

-   **`Rdbms`**：枚举，指定数据库类型（`SqlServer` / `PostgreSql`）。通过可变全局变量 `rdbms` 设置。
-   **连接 (`Conn`)**：封装了 `SqlConnection` 或 `NpgsqlConnection`。
-   **SQL 与参数**：
    -   `Sql` 记录：包含 `text` (SQL 字符串) 和 `ps` (参数数组)。
    -   `SqlParam` 联合：统一了 `SqlParameter` 和 `NpgsqlParameter`。
    -   `kvp__sqlparam`：便捷函数，从键值对创建参数。
    -   `str__sql`：将纯 SQL 字符串转换为无参数的 `Sql` 记录。
-   **结果处理**：大量使用 `Cat` 模块（`Suc<Ctx>` / `Fail<DbQueryError, Ctx>`）作为结果类型。

#### **2. 查询 (`DbQuery.fs`)**

-   **`singleline_query`**：执行 SQL 并期望**恰好返回一行**。成功则返回 `Suc<Ctx>(line)`；若返回 0 行则为 `Fail(Zero, ...)`；若返回多行则为 `Fail(OverOne, ...)`。
-   **`multiline_query`**：执行 SQL 并返回**零行或多行**。结果在 `ctx.lines` 中（`List<Object[]>`）。
-   **`singlevalue_query`**：期望返回**一行一列**。成功且值非 `DBNull` 则返回 `Some<obj>`，否则返回 `None`。
-   **`noneQuery`**：执行不返回结果集的 SQL（如 `INSERT`, `UPDATE`, `DELETE`）。

#### **3. 事务 (`DbTx.fs`)**

-   **`tx`**：直接传入连接字符串和 `Sql` 数组，自动完成打开连接、开始事务、执行、提交/回滚、关闭连接。成功则 `Suc<Ctx>` 包含 `count`（每行影响的行数）。
-   **`txOne` / `txOneSql`**：简便的事务函数，只执行单条 SQL。
-   **`PreTx<'context>`** (预处理事务)：用于**链式构建**事务。模式：
    1.  创建 `PreTx`（`opctx__pretx`）。
    2.  `pretx.sqls.Add(sql)` 追加 SQL；`pretx.sucs.Add(callback)` 注册成功回调。
    3.  `pipeline conn pretx` 一次性提交并执行所有回调。

#### **4. ORM 核心 (`Orm.fs`)**

-   **`Rcd<'p>`**：通用记录类型，包含 `ID`、`Createdat`、`Updatedat`、`Sort` 和用户自定义字段 `p`。
-   **`FieldDef`**：字段定义联合，用于生成 SQL。
-   **`MetadataTypes<'p>`**：**核心配置结构**，为每种模型实体定义一个实例，包含：
    -   `fieldorders`, `db__rcd`, `wrapper`：读取映射。
    -   `sps`：对象到 SQL 参数的转换。
    -   `id`：自增 ID 引用。
    -   `id__rcdo`：按 ID 加载。
    -   `clone`, `empty__p`, `rcd__bin`, `bin__rcd`, `p__json`, `rcd__update` 等：序列化与操作。
-   **常用 CRUD 函数**：
    -   `id__rcd`：按 ID 加载记录。
    -   `create` / `create_incremental`：插入记录。
    -   `update`：更新记录。
    -   `loadall`：按条件批量加载。
    -   `loadorcreate`：先查后插（原子化操作）。
    -   `swap_sort`：交换两条记录的排序值。
-   **批量处理与清理**：
    -   `batch`：分批处理大量记录。
    -   `merge` / `mergeAll`：合并重复记录。
    -   `checkBrokenFK` / `commitCheckBrokenFK`：检测并修复孤立外键。

#### **5. 二进制序列化 (`Orm.fs` / `Bin.fs`)**

-   提供 `rcds__bin` / `bin__rcds` 将记录集序列化为二进制格式（用于缓存或快速传输）。
-   每个字段类型都有对应的 `__bin` / `bin__` 转换函数（如 `FK__bin`, `bin__FK`）。

#### **6. 快速记忆要点（Cheat Sheet）**

| 场景 | 推荐函数 |
| :--- | :--- |
| **执行一条更新 SQL** | `txOne conn output sql_string` |
| **执行多条 SQL 原子操作** | `tx conn output [| sql1; sql2 |]` |
| **加载一条记录** | `id__rcd metadata conn id` |
| **按条件加载全部** | `loadall conn (metadata.fieldorders(), metadata.db__rcd) where` |
| **插入一条记录** | `create (conn, output, table, sps_loader) (id, time, p)` |
| **更新一条记录** | `update (conn, output, table, fieldassigns, m__ps, ...) (id, p, ...)` |
| **先查后插** | `loadorcreate (conn, output, table, id_ref, sps_loader, db__p, wrapper) (sql, p)` |
| **跨表复制/修复 FK** | `merge` / `checkBrokenFK` |
| **高性能批量导出** | `rcds__bin` / `bin__rcds` |
| **构建动态 SQL** | `build ps sql_text` 或 `str__sql sql_string` |

**下次使用时，请参考该提示以快速恢复上下文。**


以下是基于您提供的 `UtilOpen` 项目源码生成的恢复性记忆描述文档。

---

### 模块 `UtilOpen` 源码恢复记忆描述

#### 概述

`UtilOpen` 是一个 F# 库项目，旨在为应用程序提供与第三方开放平台（OAuth 认证、LLM AI、协作/社交平台等）集成的统一工具集合。其核心目标是封装各种外部服务的 API 调用，屏蔽底层 HTTP 请求和协议细节，提供简洁、类型安全且易于使用的函数接口。

项目采用模块化设计，每个平台（Clerk, DeepSeek, Discord, Google, Monday）对应一个独立的 `.fs` 文件，所有模块均位于 `UtilOpen` 的根命名空间下。项目构建于 .NET 10 之上，依赖于 `System.Net.Http`、`System.Text.Json`（隐式）、`Util` 项目以及 `Discord.Net` 和 `Microsoft.AspNetCore.Authentication.JwtBearer` 等 NuGet 包。

---

#### 核心模块描述

##### 1. `UtilOpen.Clerk` 模块 (`Clerk.fs`)

**功能描述：**
此模块专门用于处理 [Clerk](https://clerk.com/) 身份验证服务的 JWT（JSON Web Token）解析。Clerk 是一个用户管理平台，通常会在 HTTP 请求的 `Authorization` 头部提供 Bearer Token。此模块的核心功能是解析该 Token，并从中提取出 Clerk 分配的唯一用户 ID（`sub` 字段）。

**依赖项：**
- `System.IdentityModel.Tokens.Jwt`

**函数说明：**

| 函数 | 签名 | 描述 |
| :--- | :--- | :--- |
| `getClerkIdentity` | `(string -> unit) -> HttpContext -> string` | 从 `HttpContext` 的请求头中提取 Bearer Token，解析 JWT 并返回 `sub` (用户 ID)。如果验证失败或未找到 Token，则返回空字符串。第一个参数 `output` 是一个日志输出函数（当前未使用）。 |

**关键逻辑：**
1.  从 `HttpContext.Request.Headers["Authorization"]` 获取原始 Token 字符串。
2.  判断是否以 `"Bearer "` 开头。
3.  移除 `"Bearer "` 前缀，得到纯 JWT。
4.  使用 `JwtSecurityTokenHandler().ReadJwtToken(token)` 解析 JWT（无需密钥，仅读取公开字段）。
5.  从解析结果中提取 `jwtToken.Subject` 属性，该值即为 Clerk 用户 ID（例如 `user_2xb...`）。

---

##### 2. `UtilOpen.DeekSeek` 模块 (`DeepSeek.fs`) [文件名有笔误，应为 DeepSeek]

**功能描述：**
此模块封装了对 [DeepSeek](https://www.deepseek.com/) 大语言模型 API 的调用。它提供了一个异步函数，用于向 DeepSeek 的 `/v1/chat/completions` 端点发送聊天请求，并解析返回的 JSON 以提取模型生成的回复文本。

**依赖项：**
- `System.Net.Http`
- `Util.Text`
- `Util.Json`

**全局变量：**
- `client`：一个 `System.Net.Http.HttpClient` 实例，超时时间设为 5 分钟。

**私有函数：**

| 函数 | 签名 | 描述 |
| :--- | :--- | :--- |
| `loadTextFromResponse` | `string -> string` | 解析 DeepSeek API 返回的 JSON 字符串，从 `choices -> [0] -> message -> content` 路径提取文本回复。 |
| `loadErrorFromResponse` | `string -> string` | 解析错误响应，从 `error -> message` 路径提取错误信息。 |

**公开函数说明：**

| 函数 | 签名 | 描述 |
| :--- | :--- | :--- |
| `DeepSeekChat` | `(string -> unit) -> string -> string -> string -> Async<string>` | 执行异步 DeepSeek 聊天请求。参数依次为：日志输出函数 `output`，API 密钥 `apiKey`，模型名称 `model`，以及用户输入消息 `msg`。成功时返回模型回复，失败时返回空字符串。 |

**关键逻辑：**
1.  使用 `sprintf` 格式化 JSON 负载，其中 `model` 和 `msg` 被动态插入。
2.  设置 `Authorization` 头部为 `Bearer {apiKey}`。
3.  发送 POST 请求到 `https://api.deepseek.com/v1/chat/completions`。
4.  根据 HTTP 状态码和 JSON 响应内容判断是否成功，并调用对应的解析函数。

---

##### 3. `UtilOpen.Discord` 模块 (`Discord.fs`)

**功能描述：**
一个功能强大的模块，提供了与 Discord 平台交互的能力。它主要分为两部分：
1.  **OAuth2 身份验证**：用于通过 Discord OAuth2 流程获取用户信息（用户 ID、用户名、头像）。
2.  **Bot 消息操作**：基于 `Discord.Net` 库，使用 Bot Token 登录后，提供读取、发送和删除特定服务器（Guild）和频道（Channel）消息的功能。

**依赖项：**
- `Discord` (包括 `Discord.WebSocket`)
- `FSharp.Control`
- `Util.Cat`, `Util.Text`, `Util.Concurrent`, `Util.Json`, `Util.HttpClient`, `Util.HttpServer`

**OAuth2 相关函数：**

| 函数 | 签名 | 描述 |
| :--- | :--- | :--- |
| `requestAccessToken` | `(string * string) -> string -> string -> (string * string)` | 通过 OAuth2 授权码 `code` 向 Discord 请求 `access_token`。返回 `(access_token, 原始JSON响应)`。 |
| `requestUserInfo` | `string -> (string * string * string * string) option` | 使用 `access_token` 获取 Discord 用户信息。成功时返回 `(uid, username, avatar_url, 原始JSON)` 的 Option。 |
| `oauth2AuthCode` | `(string * string * string) -> string -> string` | 更简洁的 OAuth2 授权码换取 Token 函数，返回 `access_token`。 |
| `oauth2UserInfo` | `string -> (int64 option * string * string * string)` | 使用 `access_token` 获取完整用户信息，返回 `(uid option, username#discriminator, avatar_url, 原始JSON)`。 |
| `checkDiscord` | `(string * string) -> (string * string) -> (int64 option * string * string * string)` | 将 `requestAccessToken` 和 `oauth2UserInfo` 合并为一个便捷函数。 |

**Bot 消息操作函数：**

| 函数 | 签名 | 描述 |
| :--- | :--- | :--- |
| `token__client` | `string -> DiscordSocketClient` | 使用 Bot Token 创建并启动一个 `DiscordSocketClient` 实例，并等待连接完成。 |
| `loadMessages` | `(string -> unit) -> DiscordSocketClient -> uint64 -> uint64 -> IMessage[]` | 从指定的 `guildId` 和 `channelId` 异步加载所有历史消息。 |
| `sendMsg` | `(string -> unit) -> DiscordSocketClient -> uint64 -> uint64 -> (string * string) -> ((ComponentBuilder -> ComponentBuilder) option) -> uint64 option` | 向指定频道发送消息。可包含文本内容 `content` 和一个 `embedding` 描述。还支持通过可选的 `ComponentBuilder` 预处理器 `prepo` 添加交互组件。返回发送后消息的 ID。 |
| `deleteMsg` | `(string -> unit) -> DiscordSocketClient -> uint64 -> uint64 -> uint64 -> unit` | 根据消息 ID `msgId` 从指定频道删除一条消息。 |

---

##### 4. `UtilOpen.Google` 模块 (`Google.fs`)

**功能描述：**
此模块提供了与多个 Google API 交互的接口，涵盖身份验证、机器翻译以及最新的 Gemini 大模型 API。
1.  **OAuth2 登录**：通过 Google OAuth2 流程获取 ID Token。
2.  **Google 翻译**：调用 Google Cloud Translation API 进行文本翻译。
3.  **Gemini API**：封装了 Google Gemini 模型的聊天和多模态（文本+文件）调用。

**依赖项：**
- `System.Net.Http`
- `System.Text.Json`
- `Util.Text`, `Util.Json`, `Util.HttpClient`

**全局变量：**
- `client`：一个 `System.Net.Http.HttpClient` 实例，超时时间设为 5 分钟。

**类型定义：**

| 类型 | 描述 |
| :--- | :--- |
| `Part` | Gemini 请求中的单个消息部分，包含 `text`。 |
| `Content` | Gemini 请求中的单条消息内容，包含 `parts` 列表。 |
| `GeminiRequest` | Gemini 请求的顶层结构，包含 `contents` 列表。 |
| `InlineData` | 多模态请求中的内联数据，包含 MIME 类型和 base64 编码的数据。 |
| `PartMulti` | 多模态请求中的部分，可以是文本 `text` 或内联数据 `inline_data`。 |
| `ContentMulti` | 多模态请求的消息内容，包含 `PartMulti` 列表。 |
| `GeminiMultiRequest` | 多模态请求的顶层结构。 |

**函数说明：**

| 函数 | 签名 | 描述 |
| :--- | :--- | :--- |
| `requestAccessToken` | `(string[] * string * string) -> string -> string` | 通过 OAuth2 授权码 `code` 向 Google 请求 ID Token。参数为 `(client_id 数组, secret, redirect_url)`。 |
| `translate` | `string -> string -> string -> string` | 调用 Google Translate API (v2) 进行文本翻译。参数为 `apiKey`, `源语言` (未用)，`目标语言`, `要翻译的文本`。返回翻译结果。 |
| `GeminiListModels` | `(string -> unit) -> string -> unit` | 列出当前 API Key 可用的所有 Gemini 模型。 |
| `GeminiChat` | `(string -> unit) -> string -> string -> string -> Async<string>` | 标准的 Gemini 文本聊天调用。参数：`output`, `apiKey`, `model`, `msg`。返回模型生成的文本。 |
| `GeminiMultimodal` | `(string -> unit) -> string -> string -> string -> string[] -> Async<string * string>` | Gemini 多模态调用。在聊天中混入附件（图片、PDF等）。参数：`output`, `apiKey`, `model`, `msg`, `files` (文件路径数组)。返回 `(错误信息, 成功文本)` 元组。 |

---

##### 5. `UUtilOpen.Monday` 模块 (`Monday.fs`) [注意：模块名是 `UUtilOpen.Monday`，可能是笔误]

**功能描述：**
此模块专用于通过 Monday.com 官方 API (v2) 进行数据交互。所有操作均通过 POST 请求到 `https://api.monday.com/v2` 执行 GraphQL 查询。它提供了获取用户、看板（Board）及其事项（Item）的基础功能，并实现了自动分页以获取大型数据集。

**依赖项：**
- `System.Net.Http`
- `System.Text.Json`
- `Util.Json`

**全局变量：**
- `client`：一个 `System.Net.Http.HttpClient` 实例，超时时间设为 5 分钟。

**私有函数：**

| 函数 | 签名 | 描述 |
| :--- | :--- | :--- |
| `postToMonday` | `(string -> unit) -> string -> string -> string option` | 基础的 GraphQL 请求执行器。设置必要的请求头（Authorization, API-Version），发送请求，并检查 HTTP 状态码和 GraphQL 错误。成功时返回 JSON 字符串的 `Some`，否则返回 `None`。 |

**公开函数说明：**

| 函数 | 签名 | 描述 |
| :--- | :--- | :--- |
| `GetUsersRaw` | `(string -> unit) -> string -> string option` | 获取所有用户的基础信息（ID, 名称, 邮箱等），返回原始 JSON。 |
| `GetBoardItemsRaw` | `(string -> unit) -> string -> int64 -> string option` | 获取指定 Board 的 Items（单页，最多 100 条），返回原始 JSON。 |
| `GetAllBoardItemsRaw` | `(string -> unit) -> string -> int64 -> Json[]` | **重要功能**：自动分页获取指定 Board 的 **所有** Items。返回解析后的 `Json[]` 数组。此函数会递归调用，直到所有数据获取完毕。 |
| `GetBoardsRaw` | `(string -> unit) -> string -> string option` | 获取当前账户下所有 Board 的详细信息（ID, 名称, 描述, 列信息等），返回原始 JSON。 |
| `GetQueuesRaw` | `(string -> unit) -> string -> string option` | 获取看板列表（别名，用于获取“队列”），返回原始 JSON。 |
| `GetBillsRaw` | `(string -> unit) -> string -> string -> string option` | 获取特定 Board 下的 Items（别名，用于获取“账单”），返回原始 JSON。 |
| `VerifyMondayTokenRaw` | `(string -> unit) -> string -> string option` | 通过查询当前用户信息 (`me`) 来验证 API Token 的有效性，返回原始 JSON。 |






