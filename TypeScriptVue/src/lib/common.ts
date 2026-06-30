
import * as vue from 'vue'

// ===== Theme 管理 =====
export type Theme = 'day' | 'dark'

// 从 localStorage 读取初始值（兼容旧格式）
const getSavedTheme = (): Theme => {
	const v = window.localStorage.getItem('THEME')
	if (v === 'dark' || v === 'day') return v
	// 兼容旧格式（可能是 JSON 编码的字符串）
	if (v != null) {
		try {
			const parsed = JSON.parse(v)
			if (parsed === 'dark' || parsed === 'day') return parsed
		} catch {}
	}
	return 'day'
}

// 响应式 theme ref（所有组件共享）
export const theme = vue.ref<Theme>(getSavedTheme())

// ===== Lang 管理 =====
export type Lang = 'en' | 'zh' | ''

// 从 localStorage 读取初始值
const getSavedLang = (): Lang => {
	const v = window.localStorage.getItem('LANG')
	if (v === 'en' || v === 'zh' || v === '') return v
	return ''  // 默认自动检测
}

// 响应式 lang ref（所有组件共享）
export const lang = vue.ref<Lang>(getSavedLang())

// 设置 lang（更新响应式状态 + localStorage）
export const setLang = (l: Lang) => {
	lang.value = l
	window.localStorage.setItem('LANG', l)
}

// 获取实际语种（自动检测逻辑）
export const getLang = (): 'en' | 'zh' => {
	if (lang.value === '') {
		// 自动检测：根据浏览器语言
		const navLang = navigator.language || (navigator as any).userLanguage || ''
		return navLang.toLowerCase().startsWith('zh') ? 'zh' : 'en'
	}
	return lang.value
}

// 设置 theme（更新响应式状态 + localStorage）
export const setTheme = (t: Theme) => {
	theme.value = t
	window.localStorage.setItem('THEME', t)
	// 更新 document 属性（可选，便于全局 CSS 选择）
	document.documentElement.setAttribute('data-theme', t)
}

// 切换 theme
export const toggleTheme = () => {
	setTheme(theme.value === 'day' ? 'dark' : 'day')
}

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
	<User,Data>(mor: any,router: any,user:User,data:Data, defaultLang?: Lang) => {

	// 如果调用方指定了默认语言，则设置它
	if (defaultLang !== undefined) {
		setLang(defaultLang)
	}

	const rt = {
		host: initHost(),
		router: router,
		session: getLocalStorage("session", ''),
		mor: mor,
		user: user,
		data: data,
		theme: theme,
		lang: lang,
		debugger: getDebugger()
	}

	return {
		vue: vue,
		rt: rt
	}
}
