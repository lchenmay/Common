export const checkDomain = (domain:string) => {
    return [domain].includes(window.location.hostname)
}

export const buildHost = () => {
	const hostname = window.location.hostname

	const host: any = {
		hostname: hostname,
		api: `${window.location.protocol}//${hostname}`,
		wsurl: `wss://${hostname}/ws/`,
		discordAPPID: "1254790111913181274",
		discordRedirect: `${window.location.protocol}//${window.location.host}/redirect/DISCORD`,
	}

	switch (host.hostname) {
		case 'localhost':
			host.hostname = 'localhost'
			host.api = ''  // DEV: 空字符串走 Vite proxy
			host.wsurl = 'wss://localhost/'
			break
		case '127.0.0.1':
			host.hostname = '127.0.0.1'
			host.api = ''  // DEV: 空字符串走 Vite proxy
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