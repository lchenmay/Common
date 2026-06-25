module jCQT.AI.LLM

open System
open System.Collections.Generic


// ---- 消息 ----

type Role =
    | System
    | User
    | Assistant

type Message = {
    role: Role
    content: string
    mutable embedding: float[] option
}

let empty__Message() = {
    role = User
    content = ""
    embedding = None
}


// ---- 会话（单次对话） ----

type Session = {
    id: string
    messages: List<Message>
}

let empty__Session() = {
    id = ""
    messages = List()
}


// ---- 记忆画像（跨会话持久记忆） ----

type MemoryFact = {
    key: string
    value: string
    mutable embedding: float[] option
}

let empty__MemoryFact() = {
    key = ""
    value = ""
    embedding = None
}

type MemoryProfile = {
    userId: string
    facts: List<MemoryFact>
    mutable summary: string
}

let empty__MemoryProfile() = {
    userId = ""
    facts = List()
    summary = ""
}


// ---- 会话管理器 ----

type SessionManager = {
    /// 当前活跃会话
    mutable current: Session
    /// 用户记忆画像
    mutable profile: MemoryProfile
    /// 向量检索返回的 topK 条历史消息
    mutable retrieved: Message[]
}

let empty__SessionManager() = {
    current = empty__Session()
    profile = empty__MemoryProfile()
    retrieved = [||]
}


// ---- 核心操作 ----

/// 添加消息到当前会话
let addMessage (mgr:SessionManager) (msg:Message) =
    mgr.current.messages.Add msg

/// 构建发给 LLM 的完整 messages 数组（不压缩，全量发送）
let buildMessages (mgr:SessionManager) =
    let ms = List<Message>()
    // 1. 注入记忆画像作为 system 消息
    if mgr.profile.facts.Count > 0 then
        ms.Add { role = System; content = ""; embedding = None }
    // 2. 注入检索到的历史记忆
    for m in mgr.retrieved do ms.Add m
    // 3. 注入当前会话全部历史（不压缩）
    for m in mgr.current.messages do ms.Add m
    ms

/// 从当前会话提取新的事实，更新记忆画像
let updateProfile (mgr:SessionManager) =
    ()

/// 用当前消息检索相关历史记忆（RAG）
let retrieveMemory (mgr:SessionManager) =
    mgr.retrieved <- [||]


// ---- 持久化（测试阶段：内存字典） ----

let private _sessions = Dictionary<string, Session>()

/// 加载或创建会话
let loadSession (sessionId:string) =
    match _sessions.TryGetValue(sessionId) with
    | true, s -> s
    | _ ->
        let s = { empty__Session() with id = sessionId }
        _sessions.[sessionId] <- s
        s

/// 保存会话到内存
let saveSession (s:Session) =
    _sessions.[s.id] <- s

/// 从数据库加载记忆画像
let loadProfile (userId:string) =
    empty__MemoryProfile()

/// 保存记忆画像到数据库
let saveProfile (p:MemoryProfile) =
    ()

/// 将消息做 embedding 并存入向量库
let indexMessage (msg:Message) =
    ()














