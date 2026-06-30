declare global {

    namespace JSX {
        interface IntrinsicElements {
            [elem: string]: any
        }
    }

    type Host = {
        hostname: string
        api: string
        wsurl: string
    }

    interface Runtime<User,Data> {
        host: Host
        router: any
        session: string
        mor: any
        user: User
        data: Data
        theme: any 
        lang: any 
        debugger: string
    }

    interface Jlib<User,Data> {
        vue: any,
        rt: Runtime<User,Data>
    }

}

export type { Host, Runtime, Jlib }

export { }