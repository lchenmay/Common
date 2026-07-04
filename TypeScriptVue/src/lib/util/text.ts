export const decode = (src: string) => {
  let s = src.replace(/\\n/g,'\n')
  s = s.replace(/\\r/g,'\r')
  s = s.replace(/\\\\/g,'\\')
  return s
}

export const timestamp__str = (timestamp:any) => {
  let t = new Date(Number(timestamp))
  return t.getFullYear() + "/" + (t.getMonth() + 1) + "/" + t.getDate()
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

export const yyyymmdd__usa = (yyyymmdd: string) => {
  if (yyyymmdd.length != 8)
    return "";

  let yyyy = yyyymmdd.substring(0, 4)
  let mm = yyyymmdd.substring(4, 4 + 2)
  let dd = yyyymmdd.substring(6, 6 + 2)

  return mm + "/" + dd + "/" + yyyy
}

/** 美国日期 MM/DD/YYYY → YYYYMMDD（也兼容 MMDDYYYY 无分隔符和 YYYYMMDD 直传） */
export const usa__yyyymmdd = (usa: string) => {
  const clean = usa.replace(/[^\d]/g, '')
  if (clean.length !== 8) return ''

  // 优先按 MM/DD/YYYY 解析
  const m1 = parseInt(clean.substring(0, 2), 10)
  const d1 = parseInt(clean.substring(2, 4), 10)
  const y1 = parseInt(clean.substring(4, 8), 10)
  if (m1 >= 1 && m1 <= 12 && d1 >= 1 && d1 <= 31 && y1 >= 1900 && y1 <= 2100)
    return `${y1}${clean.substring(0, 2)}${clean.substring(2, 4)}`

  // 回退：用户可能粘贴 YYYYMMDD
  const y2 = parseInt(clean.substring(0, 4), 10)
  const m2 = parseInt(clean.substring(4, 6), 10)
  const d2 = parseInt(clean.substring(6, 8), 10)
  if (y2 >= 1900 && y2 <= 2100 && m2 >= 1 && m2 <= 12 && d2 >= 1 && d2 <= 31)
    return clean

  return ''
}

