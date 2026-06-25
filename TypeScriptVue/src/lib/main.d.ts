declare global {
    namespace JSX {
        interface IntrinsicElements {
            [elem: string]: any
        }
    }

    export interface Runtime {
        host: Host
        wsctx: WsCtx
        router: any
        session: string

        domainname: string
        lang: string
        user: any
        data: any
    }

    export type WsCtx = {
        ws: WebSocket
        ping: NodeJS.Timeout | null
        autoping: boolean
        pinginterval: number
        sent: number
        sentsize: number
        rec: number
        recsize: number
    }

    export type Host = {
        hostname: string
        api: string
        wsurl: string
    }

    export type NotifyItem = {
        cdate?: number
        msg?: string
        label?: string
        bgcolor?: string
        url?: string
        expire?: number
        textColor?: string
    }
}

export { }