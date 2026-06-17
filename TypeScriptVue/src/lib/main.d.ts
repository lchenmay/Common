declare global {

	var runtime: Runtime<User, ClientRuntimeData>

	export type Host = {
		hostname: string
		api: string
		wsurl: string
	}

	export interface Runtime<User, ClientRuntimeData> {
		host: Host
		session: string
		user: User
		data: ClientRuntimeData
	}

}

export { }

