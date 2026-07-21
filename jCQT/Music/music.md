# jCQT.Music 乐谱图像处理与音乐逻辑

> 图像处理实现位于 `jCQT.Music.SheetMusic`；音乐语义类型（`Key` / `TimeSignature` / `Clef`）位于 `jCQT.Music.Types`。
> 本文件从 `AIO/architecture.md` 的 §3、§4 迁移而来，专述 **PDF → segment 拼接全过程的图像处理与音乐逻辑**说明。

## 1. Music Sheet → Segment 拼接（原 §3）

### 1.1 用户视角

单页 Avalonia Tab（`TabMusicSheetToVideo`），三个按钮 + 两个下拉选项：

1. **选择 PDF**：弹出文件选择器，加载 PDF；网格视图自动按页渲染缩略图，每页带**绿色大谱表行线框**（人工核对用）。
2. **切分拼接**：对每页执行"检测→二值化→裁剪→去边→垂直对齐→横向拼接"，结果统一由 `GrandStaffSegment`（含段位图 `bmp` + 段内几何 `firstLineY`/`lineYs` + `pageIndex`/`rowIndex` + `timeSignature`/`key`）承载；输出 (a) 一张超长透明 PNG 拼接图，(b) `{pdfDir}/segments/seg-{page}-{row}.png`（如 `seg-01-01.png`=第 1 页第 1 段）各段独立图，便于按序拼接成视频。
3. **拼接视图 / 网格视图 切换**：在网格（每页独立） 与 拼接（横向长条） 间切换。
4. **分辨率 下拉**：导出保真度选项 `96 / 150 / 300 / 600` DPI，默认 300；切换后下次切分以新分辨率导出（检测/预览仍用工作分辨率 96 DPI，阈值均为相对比例，与分辨率无关）。
5. **墨色 下拉**：把二值化后的墨迹重新染成任意色，10 色可选——黑/白/红/橙/黄/绿/青/蓝/品红/浅绿，涵盖暗背景常用的亮色；切换即时对已切分结果重新着色并覆盖导出文件，无需重切。预览背景随墨色明暗自适应（暗墨→白底，亮墨→暗底 `#1E1E1E`），便于在暗背景场景评估墨色。

### 1.2 数据流

#### 主处理流程（逻辑视角）

PDF 加载为位图后，每页依次经历六个步骤：

1. **像素直方图**：每页纵横各做像素直方图，用绿线绘制在四边（`drawPageHistograms`）——底部=列暗像素分布，左侧=行暗像素分布，用于肉眼核对。
2. **水平直方图分块**：根据行级墨量直方图（`rowInk`）经滑动窗口平滑后阈值二值化，利用双行大谱表之间的空白间隙性质，确定每段 Grand staff 的垂直起止位置（`detectGrandStaffs`），完成第一步切分。
3. **段内列投影**：切分后的每段双行大谱表，各自做垂直列暗像素直方图，用绿线绘制在段底（`drawRegionProjections`），后续所有工作都在这些段切片上完成。
4. **小节线检测**：在每段上做列投影、子带双峰值、连续性、列宽严格性、结束线兜底的六判据链，检测出所有小节线并绘制蓝色竖线（`markBarLines`）。
5. **水平谱线检测**：在每段大谱表切片 `[t, b]` 上做**水平投影 + 阈值扫描**——收集 band 内所有唯一 `rowInk` 值从高到低排序，逐值作为阈值扫描连续段数，**第一个恰好露出 10 段**（大谱表：5 高音 + 5 低音）或 5 段（单行谱）的阈值即谱线位置，绘制淡紫色水平线（`drawStaffLines` → `detectStaffLinesByProjection`）。
6. **谱号区红框定位（按 5 线切上下谱表）**：先由 5 线定位（`detectStaffLinesByProjection` 填充各段 `lineYs`，升序 10 线 = 高音 5 + 低音 5）把大谱表拆成上下两个谱表（每行谱各一个红框）。**左/上/下 = 用户硬性规定确定性取值、上下谱表共用同一份**：`xLeft = bx0`（大括号左缘）、`xTop/xBot = s0-L .. s4+L`（按 5 线纵向切分）。每个 region 只算一次 `xL/上/下`，仅按 5 线做纵向切分，因此上/下谱号红框**必定等宽、且绝不因谱号墨迹形态（如低音谱号断口）退化成直线**。不读像素、不做列墨量投影。**右边界（xRight）不再固定为 `bx0 + 0.5 × braceH`，而由「高斯模糊 + 堆叠共识」精修**——在保持左/上/下不变的前提下，把"谱号/调号/拍号"与"后继小节/音符"切开（算法见 §1.3）。`K<3` 或共识可疑（过窄）时回退到确定性宽度 `0.5 × braceH` 兜底，保证铁律不退化。大谱表出上下两个谱号红框、单行谱出 1 个、<5 线跳过（`detectClefRegions` → 红框由 `drawRegionBoxes` 绘制）。

> **与代码渲染顺序的关系**：实际代码 `renderPage` 中的绘制顺序是 6→5→4→3→1（红框→谱线→小节线→段投影→页直方图），这是由图层叠加决定的（底→面）。逻辑检测顺序：步骤 2（`detectGrandStaffs`）在 `renderAll` 中先于 `renderPage` 执行，结果缓存复用。

### 1.3 谱号/调号右边界：高斯模糊 + 堆叠共识（stack & consensus）

**目标**：在保持红框左/上/下不变的前提下，把"谱号 + 调号 + 拍号"这块**常数信号**与"后继小节/音符"这块**随机信号**切开，得到精确右边界；同时让 `markBarLines` 的头部排除区不再盲目用左侧 4% 页宽，而是从该右边界起算。

**核心观察**：同一页 K 个系统（大谱表带）里——
- **常数信号**：谱号、调号、拍号，逐系统几乎逐像素相同（高音谱号/低音谱号各自相同，调号符号相同）。
- **随机信号**：每个系统的音符、连音线、歌词位置各不同；小节线虽贯穿全高，但各系统横向位置不同。

把 K 个带对齐后逐像素叠加，**常数保留、随机被平均掉**，谱号调号区便从噪声中"浮现"（天文学消宇宙射线、文档分析抹可变批注的同款手法，称 stack & consensus）。

**算法（`detectClefKeyRightByConsensus`，返回相对 xL 的像素右缘；`detectClefRegions` / `detectClefKeyTimeRegions` 复用）**：

1. **取带**：同页 K 个谱表带（每 region 按 lineYs 每 5 线切成高音/低音两条，即 `bands`）。每带记录 `(xL=bx0, yTop, yBot, staffYs)`。
2. **对齐**：
   - 垂直：各带"垂直完全相同"，按 `yTop` 相对参考带偏移对齐（预期偏移≈0）。
   - 水平：以 `xL`（=bx0 大括号左缘）为锚对齐，**吸收各系统"略有偏移"**，无需像素级互相关。
3. **轻量高斯模糊**：每带裁成 luma 数组后做可分离二项式 5 抽头核 `[1,4,6,4,1]/16`（σ≈1.0）模糊。作用有二——吸收垂直/水平微小坐标差；常数信号保留、随机音符被抹弱（模糊后孤立音符显著性骤降，常数块依旧显著）。
4. **逐像素"全带一致墨"投票**：对每带每像素，若模糊后 luma<150 记"有墨"。累加 `inkVote[r,c]`（有墨带数）与 `bandCnt[r,c]`（参与带数），但**排除谱线行（±1px，由 `staffYs` 生成掩膜）**，避免横贯全宽的谱线污染逐列统计。
5. **逐列一致比例**：`ratio[c] = 非谱线行中"全带一致有墨"的行数 / 有效行数`。谱号调号区每带都有墨 → ratio≈1；音符区仅个别带有墨 → ratio 低；小节线各系统位置不同 → ratio 低。
6. **取最左连续一致块**：找 column 0 起、满足 `ratio ≥ 0.5`（高阈值起始）、允许内部 `ratio ≥ 0.3` 桥接 ≤10px 间隙（谱号与调号间的窄缝）的连续段，其右缘 +2px 余量即相对 xL 的 `xR`。`xLeft=0`（对齐锚）的块才被接受，避免误抓后方孤立墨团。

**共存与兜底（不破坏铁律）**：
- 红框**左=xL=bx0、上/下=按 5 线切分**，完全沿用确定性规则，绝不退化。
- 右边界 `xR`：共识可信（`xR_rel ≥ max(24, 0.12×braceH)`）时取 `xL + xR_rel`；否则回退确定性 `bx0 + 0.5×braceH`。
- **样本不足（`K<3`）或无一致块**：直接回退原单图列投影 `legacyClefKeyTimeRegions`（头部块）与确定性 `0.5×braceH`（红框），零回归风险。

**下游生效**：`detectClefKeyTimeRegions` 返回的头部块右边界被 `markBarLines` 用作每系统小节线检测的起始排除列（`max(braceZone, headRight+8)`），于是小节线不再从谱号调号区内部冒出——"谱号调号"与"后继小节"在结构与视觉上均被分开。



```
PDF 文件
  │
  ├─▶ 检测/预览：PDFtoImage.Conversion (工作分辨率 DPI 96)
  │       │
  │       ▼  SheetMusic.detectGrandStaffs   ←── 本模块核心算法（阈值均为相对比例，与 DPI 无关）
  │       List<StaffRegion> per page          (topY, bottomY, firstLineY, lineYs，工作分辨率坐标)
  │
  └─▶ 导出：PDFtoImage.Conversion (高分辨率 DPI，由界面「分辨率」下拉选择，默认 300)
         │
         ▼  整页立即 Phase 0 二值化（空白→透明、墨迹黑+alpha，RGB 不变）
         SKBitmap (高分辨率整页，透明背景 + 黑墨 alpha 遮罩)

SKBitmap (高分辨率整页) + List<StaffRegion>(工作分辨率坐标)
  │     坐标按 ratio=DPI_out/DPI_work 缩放到高分辨率
  ▼  SheetMusic.segmentPage   (逐段：cropRegion 裁出 topY~bottomY + trimMargins 去左右白边，
  │                            并把 firstLineY/lineYs 换算到段内坐标 → 产出 GrandStaffSegment)
  │     每个 segment 自带 bmp + firstLineY + lineYs + pageIndex/rowIndex + timeSignature/key(占位)
  │     ⚠️ 此处裁剪在"已二值化"的整页上做，故段位图天然是"黑墨+透明"，无需再二值化
List<GrandStaffSegment>  (每页一段，跨页收集；段内几何自描述，便于拼接/回溯)
  │
  ▼  SheetMusic.alignVertical  (按 firstLineY 对齐首线，lineYs 同步平移；返回对齐后的 segment)
List<GrandStaffSegment> (高度统一、首线齐平，黑墨+透明，高分辨率)
  │
  ▼  SheetMusic.cropRange  (拼接前切除谱号/调号区：仅保留全局第一个切片，
  │                       其余切片按段内 clefRightX(红框右) 到 rightBarX(最右小节线) 水平裁取；
  │                       clefRightX/rightBarX 由 detectClefRightByRegion / markBarLines 经 ratio 升采样后映射，见 §1.5)
List<GrandStaffSegment> (除首段外均不含左侧谱号/调号区，黑墨+透明，高分辨率)
  │
  ├─→ SheetMusic.recolorInk   (按 UI 选定墨色把 alpha 遮罩覆写为任意色；透明背景不变。
  │                            墨色含暗背景常用亮色：白/黄/青/品红/浅绿/橙/蓝等，默认黑)
  │     └─→ SheetMusic.stitchHorizontally →  拼接视图（UI 展示，预览背景随墨色明暗自适应：
  │     │                                    暗墨→白底，亮墨→暗底）
  │     └─→ SheetMusic.saveSegments       →  segments/seg-{page}-{row}.png（按 segment 的
  │                                           pageIndex/rowIndex 命名，无框、高分辨率、便于按序拼接成视频）
  └─（基础分段以"黑墨+透明"的 GrandStaffSegment 保留于内存，切墨色时无需重新切分，
      仅对 .bmp 重新 recolorInk 即可）

### 1.1 `GrandStaffSegment`：切片分行结果的最终载体

`SheetMusic.segmentPage` 把一页的 `StaffRegion list` 转为 `GrandStaffSegment list`，作为"PDF→图片→切片→分行"全过程的**最终结果载体**。它把"图像结果"与"几何/元数据"打包进一个自描述对象，使后继拼接、回溯、以及未来的音乐解析都无需再回查 `StaffRegion`：

| 字段 | 类型 | 说明 |
|------|------|------|
| `pageIndex` | `int` | 段在原始 PDF 的页码（1-based），拼接/回溯来源 |
| `rowIndex`  | `int` | 段在页内的行序号（1-based） |
| `timeSignature` | `TimeSignature`（mutable） | 拍号，由谱号/调号/拍号区检测后回填，默认 4/4 占位 |
| `key` | `Key`（mutable） | 调号（大调+关系小调），检测后回填，默认 C 大调 / a 小调占位 |
| `firstLineY` | `int` | 上五线谱首线在 `bmp` 内的 Y（px），`alignVertical` 的对齐基准 |
| `lineYs` | `int list` | 该段全部五线谱横线 Y（相对 `bmp`），用于谱线预览与后续解析对齐；`alignVertical` 会同步平移 |
| `bmp` | `SKBitmap` | 该段大谱表裁剪位图（已二值化+去边，透明背景） |
| `clefRightX` | `int option` | 已识别谱号/调号区右界（段内位图局部 x，px）。第一个切片为 `None`（保留谱号调号），其余为 `Some xR`，拼接前从红框右边框起切除左侧谱号/调号区（见 §1.5） |
| `rightBarX` | `int option` | 该切片最右小节线位置（段内位图局部 x，px）。第一个切片为 `None`；其余为 `Some bx`，与 `clefRightX` 配合把切片收口到最右小节线（见 §1.5） |

> 设计要点：早期 `saveSegments` 入参是 `(SKBitmap * (int*int))` 元组，元数据与位图分离，且文件名用三位序号、丢失页码/行号。改用 `GrandStaffSegment` 后：(a) 切分结果天然携带来源坐标与段内几何；(b) 拼接时直接取 `.bmp`、对齐时读 `.firstLineY` 并写回 `.lineYs`；(c) 保存用 `pageIndex/rowIndex` 命名，便于跨页按序拼接与视频生成。
```

> 预览分支（`renderPage`）同样把黑墨重染为选定墨色，再叠加检测框线，最后按墨色明暗
> 合成到暗/亮底（`scaleBitmap ?bg`），便于在暗背景场景评估墨色表现。

预览阶段还有一个并行分支：

```
SKBitmap (per page) + List<StaffRegion>
  │
  ▼  SheetMusic.detectBracesFromBitmap   (大括号检测)
  ▼  SheetMusic.detectClefKeyTimeRegions  (谱号/调号/拍号区检测)
List<(xCenter, yTop, yBot)>
  │
  ▼  SheetMusic.drawRegionBoxes          (绿框+蓝框叠加到页面图)
  ▼  SheetMusic.markBarLines             (在页面上画蓝色小节线竖线)
  ▼  SheetMusic.drawRegionProjections    (每段底部叠加绿色列投影)
  ▼  SheetMusic.drawPageHistograms       (在页面四边叠加绿色像素直方图)
  ▼  SheetMusic.scaleBitmap ×2           (整体 2x 放大，预览更清晰)
WrapPanel 单格控件 → 网格预览
```

### 1.3 谱号/调号/拍号区检测 `detectClefKeyTimeRegions`（红框用，原 §3.2.0）

- **目的**：在每段 Grand staff 左侧叠加红色框，标出"谱号+调号+拍号"占据的**估算**范围（不参与切分/拼接，仅可视化）。
- **算法**（简化版 v8，大括号锚定 / 估算定位）：
  1. **复用大括号检测**：直接吃 `renderPage` 已算好的 `braces`（来自 `detectBracesFromBitmap`，内部 `detectBracesForRegions` 返回 `(xStart, xEnd, yTop, yBot)`，每段必有一个 brace，左缘即真实大括号左缘）。
  2. **红框左边** = `braces.[i].xStart`（紧贴大括号，不再用脆弱的 CCA/NCC 模板匹配，杜绝漏检/错位）。
  3. **红框宽度** = 大谱表高度 × 0.5：`xR = bx0 + int(0.5 × braceH)`，其中 `braceH = byB − byT`（= 大括号贯穿 10 线的高度）；无 brace 时用 region 高度兜底。
  4. **红框上下** = 高音谱表最高线 → 低音谱表最低线（`min/max lineYs`）。每段必出一框（零漏检）；`braces` 比 `regions` 短时补零兜底（全宽回退）。
- **返回值**：与 regions 一一对应的 `(xL, xR, yT, yB)` 矩形，由 `drawRegionBoxes` 绘制红色线框。
- **设计取向**：先按大括号+0.5 宽度做**估算定位**（保证不漏检、不误检），若后续需要更贴合末位升降号，可在此基础上用 NCC 取 MAX 微调（可选增强，非必须）。

### 1.4 小节线检测 `markBarLines`（可视化用，原 §3.2.1）

- **目的**：在网格预览的每段大谱表上叠加蓝色竖线，便于肉眼核对小节边界（不参与切分/拼接，仅可视化）。
- **入口**：`SheetMusic.markBarLines (output) (page) (content) (regions) (_clefRegions)`，由 `TabMusicSheetToVideo.renderPage` 在 `drawRegionBoxes` 之后调用，传入 `runtime.output`（Avalonia `TextConsole` 日志组件，即主程序底部日志区）。
- **算法（判据链 v6：列投影 + 子带双峰值 + 连续性 + 列宽严格性 + 结束线兜底）**：
  - 判据1：列暗像素投影 `colInk` 局部峰值（≥ `0.3×bandH`）提名候选列。
  - 判据2：把 band 在竖直中点一分为二（上支/下支），要求 `上支colInk ≥ 0.3×上支高 且 下支colInk ≥ 0.3×下支高`。小节线上下两支都有强墨；音符符干/和弦只占单支谱表高度，被滤掉。
  - 判据3：连续性——列中最长连续暗像素段 `maxRun ≥ 0.4×bandH`。真实小节线是 1px 宽的连续竖线，在绝大多数 band 高度上都有墨（`maxRun` 接近 `bandH`）；跨双谱表的"和弦符干"虽能过判据2，但符干只跨 1~2 个八度、notehead 与 stem 间有空隙，最长连续段 ≪ `bandH`，被滤掉。**注意**：阈值 `0.4` 针对"贯通整段大谱表"的竖线；但密集 16/32 分音符段，notehead 横向紧密到近乎连续，整列 `maxRun` 也能接近 `bandH`——**判据3 在这种情况下失效**，必须靠判据5 兜底。
  - 判据4：相邻小节线距离下限 `minDist = max 8 (0.012×w)`，避免同根线被重复标记。
  - 判据5（列宽严格性）：真实小节线是孤立 1~2px 竖线，候选列的"有效列宽"（以 50% 峰值为阈值向左右扩展到该阈值以下）必须 ≤ 2px；单 notehead 列宽 3~5px、16/32 分 notehead 串列宽 6~15px、4/4 宽和弦块列宽 ≥ 20px，全部被滤掉。**关键优势**：完全不依赖 ±N 邻域范围（v4 设计的 ±2..5 区正好覆盖了相邻 notehead 中心被污染，判据5 失效），仅测本候选列自身阈值以上的左右扩展宽度，直接对应"是不是 1~2px 孤立竖线"。阈值从 v5 的 **30% 提升至 50%**：某些小节线恰与五线谱某条横线相交，横线在 ±1 列贡献约 5~10px 墨，30% 阈值（≈峰值/3）会把这类邻列纳入 → 列宽扩到 3~4px 误杀真实小节线；50% 阈值下横线穿越贡献远低于半峰值，扩不过去（`maxLineWidth=2`，可调到 `3` 容纳渲染偏差）。该判据**专治密集音符段漏网**，是第4页根除方案的核心。
  - 判据6（结束线兜底，v6 新增）：结束线通常是双竖线（细线+粗线），本身宽 4~6px，`lineWidth >2` 会被判据5 误杀。在段最右端 **15%** 宽度内，凡过判据1~3 的候选列、且未入选判据5 的，取**最右那根**作为结束线追加绘制。仅兜底、不扩散，避免把段内右侧密集音符误判。
  - 排除最左侧 brace 区（`max 20 (0.04×w)` 页宽）。
- **调试日志**：每段输出 `[小节线] 段#ri bandH/上支高/下支高 候选/判据2-3/列宽过滤后/结束线兜底/绘制 数量`（其中"判据2-3"=过判据2+判据3 的列数，"列宽过滤后"=再经判据5 剩下的列数，"结束线兜底"=判据6 追加的列数）；全部段完成后输出 `[小节线] 完成：N 段共绘制 M 根蓝色竖线`。日志写入主程序日志区（Avalonia 日志组件），**不使用 printfn**。

### 1.5 拼接前切除谱号/调号区（红框右 → 最右小节线，`clefRightX` + `rightBarX` + `cropRange`）

- **目的**：长谱拼接时，只有**全局第一个切片**需要保留谱号/调号/拍号（供读谱），其余切片都已自带上下文，左侧谱号/调号区纯属重复、且会引入大量空白导致拼接错位。故在 `alignVertical` 之后、`recolorInk` 之前，把**第二个切片起**的所有切片从"红框右边框"水平切取到"该切片最右小节线"，切除左侧谱号/调号区、并收口右侧到小节线。
- **左界来源（红框右 = `clefRightX`）**：`detectClefRightByRegion`（与 §1.3 红框 `detectClefRegions` **完全同源**，构造相同的谱表带、跑同一 `detectClefKeyRightByConsensus`）：按 region 分组取该 region 各谱表带的最大 `xR = bx0 + xrRel`（共识不可用/过窄时回退 `bx0 + 0.5×braceH`）。返回与 `regions` 一一对应的页面级右界（工作分辨率），在 `runSplit` 内乘以 `ratio` 升采样为 `clefRightHi`；`segmentPage` 内 `cropRegion` 全宽起步（列 0 = 页面 x=0），`trimMargins` 砍左空白 `left = leftInkColumn cropped`，段内左界 = `clefRightHi[idx] - left`，存入 `clefRightX`。
- **右界来源（最右小节线 = `rightBarX`）**：`runSplit` 内对每个 region 调 `markBarLines`（`clefRegions` 头部块 = 该 region 红框右界，用于排除谱号/调号区），取每 region 的 `barLines`（页面级 x 列表），最右小节线 = `max(barLines)`；乘以 `ratio` 升采样为 `rightBarHi`，段内右界 = `rightBarHi[idx] - left`，存入 `rightBarX`。小节线检测自带结束线兜底（段最右 15% 取最右过判据列）→ 右界即谱表尾。
- **裁剪**：`runSplit` 在 `alignVertical` 后遍历 `croppedSegs`，跳过全局 `i=0`；其余当 `clefRightX = Some cx` 且 `rightBarX = Some bx` 且 `bx > cx+2` 且 `bx < bmp.Width` 时，用 `cropRange bmp cx bx` 裁出 `cx→bx` 子图（垂直对齐不受水平裁剪影响，谱线仍齐平）；退化情形（仅有 `clefRightX`、无小节线）用 `cropLeft bmp cx` 兜底裁到右侧；两者皆无则跳过并打日志。每个非首切片均打印 `切片# 页 谱号右界relCx / 最右小节线relBarX / 裁后宽` 供核对。
- **依赖铁律**：左界的"左/上/下确定性 + 右共识精修"与 §1.3 红框完全一致（`detectClefRightByRegion` 与 `detectClefRegions` 构造相同谱表带 → 共识 `xrRel` 一致），红框预览与拼接裁剪不会 discrepancy；右界来自已验证的小节线检测，与红框同源排除头部块。

### 1.6 关键文件（原 §3.3）

| 文件 | 行数 | 职责 |
|---|---|---|
| `AIO.BizLogics/SheetMusic.fs` | ~1316 | 所有乐谱处理算法（检测、裁剪、二值化、对齐、拼接、可视化）|
| `AvaloniaApp/Comp/TabPage/TabMusicSheetToVideo.fs` | ~372 | Tab UI + 流程编排 + 缓存（含 DPI/墨色下拉与暗背景预览）|

`SheetMusic.fs` 模块内的 8 个子模块按从底到顶的依赖：

```
1. 底层像素工具     copyPixels
2. 5 线检测         detectStaffLinesByProjection (阈值扫描，被 detectGrandStaffs 唯一调用)
3. 大谱表行检测     detectGrandStaffs    ★ 唯一入口
4. 大括号检测       detectBracesForRegions
5. 二值化           binarizeTransparent（Phase 0，整页级，仅改 alpha，检测照常）
6. 切片+分行        segmentPage（cropRegion 裁剪 + trimMargins 去边 + firstLineY/lineYs 几何换算 → GrandStaffSegment）
7. 对齐+拼接        alignVertical（按 firstLineY，返回 GrandStaffSegment）/ stitchHorizontally
8. 保存             saveSegments（按 pageIndex/rowIndex 命名 segments/seg-{page}-{row}.png）
9. 可视化           drawRegionBoxes / markBarLines / drawRegionProjections / drawPageHistograms / scaleBitmap
```

## 2. 核心算法：大谱表行检测（`detectGrandStaffs`，原 §4）

### 2.1 设计思想

**5 线系统（单行谱或钢琴大谱表）横贯整行**——5 根谱线每根都跨满整个乐谱宽度，每根线在水平直方图里都贡献一个高耸的"墨量峰"，远强于符头、歌词、谱号等局部元素。利用这一几何特征，可以用**水平投影 + 阈值扫描**直接读出谱线 Y 坐标，**完全不需要 Hough 变换或连通分量**等 2D 方法。

> 历史：v1 用霍夫变换（θ∈[87°,93°]）做谱线检测，因表情记号/连音线在近水平方向的票数也够、覆盖率过滤扛不住，频繁误检。v2 改用连通分量（4-连通 + Union-Find）选在 clef/key 窄条，靠几何判据（宽 ≥ 0.6×stripW ∧ 高 ≤ 4px）排除干扰，但要求预先定位窄条范围。**v3（当前）**用 1D 投影 + 阈值扫描，逻辑最简、最稳。

### 2.2 三阶段流水线

```
┌─────────────────────────────────────────────────────────────┐
│ 阶段 A：行级墨量直方图                                       │
│   rowInk[y] = 该行 luma<150 的暗像素数                       │
└─────────────────────────────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────┐
│ 阶段 B：滑动窗口平滑（窗口 = 2% h，桥接大谱表内部高音-低音间│
│         缝，同时保留系统间空隙为真低谷）                       │
│   rowInkS[y] = mean(rowInk[y-r..y+r])                        │
└─────────────────────────────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────┐
│ 阶段 C：阈值二值化 + 连续行分块                              │
│   isInk(y) = rowInkS[y] ≥ 12% × w                           │
│   分块：连续 isInk 行；间隔 > 3% h 视为新块                  │
│   过滤：块高 < 1.6% h 视为噪点丢弃                          │
└─────────────────────────────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────┐
│ 阶段 D：高度分布 outlier 检测 + 自动拆分被合并的双行         │
│   medianH = 所有块高度的中位数                                │
│   mergedThresh = 1.65 × medianH                              │
│   高度 > mergedThresh 的块 = 疑似 2 个大谱表行被合并         │
│   → 切分：在块正中 ± blockH/4 内找最低墨量行作切点           │
│   → 切分后两半均 ≥ minBlockH 才接受切分                       │
└─────────────────────────────────────────────────────────────┘
                            │
                            ▼
                  List<StaffRegion> (按 topY 排序)
```

### 2.3 阈值一览（全部按页面高度 h 比例，无外部参数）

| 阈值 | 公式 | 几何意义 |
|---|---|---|
| `smoothWin` | `2% × h` | 桥接大谱表内高音/低音谱之间窄缝（≈1 线距）。明显大于内缝、明显小于系统间缝（多个线距）。 |
| `inkFrac` | `12% × w` | "有乐谱墨迹"行判定：谱线横贯整行 → 高占比；零散音符/文字 → 低占比。 |
| `gapThresh` | `3% × h` | 块间空隙判据：大于此值视为新块开始。 |
| `minBlockH` | `1.6% × h` | 最小保留块高：约 4 个线间距。保住单行谱系统（《Très animé》等密集行）。 |
| `mergedThresh` | `1.65 × medianH` | 双行合并嫌疑门槛：单行 ≤ 1×，双行 ≈ 2×，1.65 居中。 |
| 切分搜索半径 | `blockH / 4` | 块内谷点搜索范围：以中点为中心，保证每半 ≥ blockH/4 ≈ medianH/2。 |
| 切分 sanity | `半高 ≥ minBlockH` | 切出后任一半太小则放弃切分（防误切正常块）。 |

**为什么用中位数而非均值？** outlier（合并块）高度 ≈ 2×，会拉高均值但不影响中位数，门槛更鲁棒。

**为什么用原始 `rowInk`（未平滑）找谷点？** 平滑会把"系统间空隙"也填上一部分，谷点被削弱；原始 `rowInk` 保留最深低谷。系统内部空隙（高/低谱之间）虽也存在，但该处常有大括号 brace 提供墨量，所以最深低谷几乎一定落在"系统间空隙"上。

### 2.4 复杂度

每页 O(h × w) 像素扫描（阶段 A 一次、阶段 C 一次 1D 扫描） + O(h) 平滑 + O(B log B) 高度排序（B = 块数，远小于 h）。

实测 96 DPI 渲染的 A4 页面（≈ 800×1100 像素）在普通笔记本 < 50ms。

### 2.5 调试钩子

```bash
# Windows PowerShell
$env:HOUGH_DEBUG = "1"  # 控制台打印分块切分日志（阶段 C/D）
dotnet run --project AvaloniaApp
# 控制台会打印：
#   [hough] histRows(before split)=N -> [(t1,b1), (t2,b2), ...]
#   [hough] split outlier h=X (median=Y, thresh=Z) -> 2 pieces [...]
#   [hough] histRows(after split)=M
```

### 2.6 段内谱线检测：水平投影 + 阈值扫描（原 §4.6）

`detectGrandStaffs` 阶段 D 输出每个 band 的 `[t, b]`，对每个 band 调用 `detectStaffLinesByProjection` 在原始 `rowInk[t..b-1]` 上做水平投影阈值扫描：

```
                     10 根线 = 大谱表（高音 5 + 低音 5）
                      5 根线 = 单行谱

rowInk 分布（示意）：
  ↑
  │  ┌┐   ┌┐   ┌┐   ┌┐   ┌┐    ←─ 阈值 90%：5 根高音谱线
  │  ││   ││   ││   ││   ││
  │──┘└───┘└───┘└───┘└───┘└────  ←─ 阈值降至 70%：低音谱线也露出
  │  ┌┐   ┌┐   ┌┐   ┌┐   ┌┐
  │  ││   ││   ││   ││   ││
  │──┘└───┘└───┘└───┘└───┘└────  ←─ 阈值 60%：连续段数 = 10（大谱表 ✓）
  │    ┌┐   ┌┐
  │  ~~ ││  ││  ~~ 符头等噪点开始混入
  │    ┌┐   ┌┐
  │── 阈值继续降 → 段数 > 10，停止
  └──────────────────────────────→ y
```

**算法步骤**（`detectStaffLinesByProjection rowInk t b`）：

1. **阈值阶梯**：收集 band 内所有唯一 `rowInk` 值，去重降序排序 → `thresholds[]`（约几百个阶梯）。
2. **段统计** `segmentsAt thr`：扫描 `y ∈ [t, b)`，数 `rowInk[y] ≥ thr` 的连续段，每段取 `(segStart + segEnd) / 2` 作为候选线 Y。
3. **目标匹配** `tryTarget n`：`thresholds |> Array.tryPick`，**第一个**让段数恰好等于 `n` 的阈值返回其段中点列表。
4. **优先大谱表**：`tryTarget 10` 优先；找不到（如单行谱）回退 `tryTarget 5`；都没有则返空。

**为什么这个方法可靠**：

- 谱线横向贯穿整页 → 每行暗像素数 `rowInk` 在谱线位置是全页最强峰（远高于符头 ~30px 宽、符干 ~1px 宽、谱号 ~50px 宽但只占左 ~4% 宽度），所以从高阈值往下走，**谱线必然最先被捕获**。
- 谱线之间是被真正的"系统内部空隙"分隔的（高/低谱之间的 brace 区 ~0 ink，或单行谱的两线之间 ~0 ink），所以 `rowInk` 在谱线之间会跌到很低 → 阈值不会"漏并"相邻谱线。
- 不需要等距假设、不需要 Hough 投票、不需要连通分量几何判据、不需要预先定位窄条。

**复杂度**：O(b - t) 像素扫描 + O(B log B) 排序（B = 阈值数 = band 高度内不同 ink 值的个数） + 单次 `tryPick` 命中即停（最坏 case 才扫完所有阈值）。实测 96 DPI 单 band 几 ms。

**调用位置**：`detectGrandStaffs` 第 453 行，传入 `rowInk t (b - 1)`，返回的 `lineYs` 经 `List.sort` 后写入 `StaffRegion.lineYs`，由 `drawStaffLines` 画淡紫色水平线叠加在页面上供肉眼核对（见截图：4 页大谱表的 10 根线全部精确对齐）。
