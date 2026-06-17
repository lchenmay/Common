declare global {
    namespace globalThis {
        var host: Host
        var runtime: Runtime
    }

    export interface Runtime<User,ClientRuntimeData> {
        host: Host
        wsctx: WsCtx
        router: Router
        session: string

        domainname: string
        lang: string
        user: User
        data: ClientRuntimeData
    }

    export type Host = {
        hostname: string
        api: string
        wsurl: string
    }

}

export { }

