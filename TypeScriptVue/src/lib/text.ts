
export const s__limitLength = (length: number) => (s:string) => {
  if(s.length <= length)
    return s
  else
    return s.substring(0,length)
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

