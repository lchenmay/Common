
import * as vue from 'vue'

export const getDebugger = () => {

// 判断调试来源：前端端口 % 4 === 3 → AI，否则 → human
	const _port = parseInt(window.location.port) || (window.location.protocol === 'https:' ? 443 : 80)
	return (_port % 4 === 3) ? 'ai' : 'human'
}

export const is_local = () => {
    return ["127.0.0.1", "localhost"].includes(window.location.hostname)
}

export const checkDomain = (domain:string) => {
    return [domain].includes(window.location.hostname)
}

export const initHost = () => {
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

// 创建 jlib 的工厂函数
// 应用可以传入自己的 runtime 来覆盖默认值
export const createJlib = 
	<User,Data>(mor: any,user:User,data:Data): Jlib<User,Data> => {

	const rt = {
		host: initHost(),
		router: null,
		session: getLocalStorage("session", ''),
		mor: mor,
		user: user,
		data: data,
		debugger: getDebugger()
	}

	return {
		vue: vue,
		rt: rt
	} as Jlib<User,Data>
}
