export const decode = (src: string) => {
  let s = src.replace(/\\n/g,'\n')
  s = s.replace(/\\r/g,'\r')
  s = s.replace(/\\\\/g,'\\')
  return s
}

export const timestamp__str = (timestamp:any) => {
  let t = new Date(Number(timestamp))
  return t.getFullYear() + "/" + t.getMonth() + "/" + t.getDay()
      + " " + t.getHours() + ":" + t.getMinutes()
}

export const url__param = (name: string) => (url: string):string => {
const regex = new RegExp("[?&]" + name + "=([^&#]*)", "i")
const match = regex.exec(url)
if(match)
  return decodeURIComponent(match[1])
else
  return ""
}

export const __s = (str:string) => {
  let items = JSON.parse(str)
  return items[(globalThis as any).runtime?.lang]
}


export const s__limitLength = (length: number) => (s:string) => {
  if(s.length <= length)
    return s
  else
    return s.substring(0,length) + ' ...'
}

export const amt__2digitDollar = (num: number): string => {

  if (isNaN(num)) return '$0.00'

  const sign = num < 0 ? '-' : ''
  const absNum = Math.abs(num)
  const formatted = absNum.toLocaleString('en-US', {
    minimumFractionDigits: 2,
    maximumFractionDigits: 2
  })

  return `${sign}$${formatted}`
}

