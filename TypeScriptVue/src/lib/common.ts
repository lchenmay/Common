

export const buildHost = () => {
	const hostname = window.location.hostname

	const host: Host = {
		hostname: hostname,
		api: `${window.location.protocol}//${hostname}`,
		wsurl: `wss://${hostname}/ws/`
	}

	switch (host.hostname) {
		case 'localhost':
			host.hostname = 'localhost'
			host.api = 'https://localhost'
			host.wsurl = 'wss://localhost/'
			break
		case '127.0.0.1':
			host.hostname = '127.0.0.1'
			host.api = 'http://localhost'
			host.wsurl = 'wss://localhost/'
			break
	}

	return host
}

export const getLocalStorage = (key: string, defaultv: string = '{}') => {
	const v = window.localStorage.getItem(key)
	if (v != undefined) {
		try { return JSON.parse(v) }
		catch {
			if (v) return v
			else return defaultv
		}
	}
	return defaultv
}

export const setLocalStorage = (key: string, value: any) => {
	window.localStorage.setItem(key, JSON.stringify(value))
}

export const empty__Runtime = <User, ClientRuntimeData>(): Runtime<User, ClientRuntimeData> => {

	let runtime = {
		host: buildHost(),
		session: getLocalStorage("session", ''),
		user: {} as User,
		data: {} as ClientRuntimeData
	} as Runtime<User, ClientRuntimeData>

	return runtime
}