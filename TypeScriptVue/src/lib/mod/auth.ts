
type authParams = {
  client_id: string
  response_type: string
  redirect_uri: string
  scope: string
}
type LoginOption = {
  biz: string
  code: string
  redirectUrl: string
}

export const SignOut = () => {
  window.localStorage.clear()
  location.reload()
}

export const biz__LoginOptions = (biz: string = "DISCORD"): LoginOption => {
  const options = {
    biz: biz.toUpperCase(),
    code: "",
    redirectUrl: (globalThis as any).runtime?.host?.discordRedirect,
  }

  const urlParams = new URLSearchParams(window.location.search)
  options.code = urlParams.get('code')!
  return options
}

export const LoginOption__RT = async (options: LoginOption, postFn: Function, notifySuc: Function) => {
  const res = await postFn('/api/public/auth', options)
  if (res.session) {
    if (notifySuc) notifySuc("Login Suc")
    ;(globalThis as any).runtime.session = String(res.session)
    if(res.ec) (globalThis as any).runtime.user = res.ec
  } 
  return res
}

const authParams__URLQuery = (params: Record<string, any>) => {
  return Object.keys(params)
    .map(key => encodeURIComponent(key) + '=' + encodeURIComponent(params[key]))
    .join('&');
}

export const host__DiscordRedirectURL = (host?: any) => {
  const h = host || (globalThis as any).runtime?.host
  const p: authParams = {
    client_id: h?.discordAPPID,
    response_type: "code",
    redirect_uri: h?.discordRedirect,
    scope: "identify"
  }
  const params = authParams__URLQuery(p as any)
  return `https://discord.com/oauth2/authorize?${params}`
}