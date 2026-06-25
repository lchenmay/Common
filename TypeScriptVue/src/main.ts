import { createApp } from 'vue'
import App from './App.vue'

;(globalThis as any).runtime = {
  host: { hostname: 'localhost', api: '', wsurl: '' },
  wsctx: null,
  router: null,
  session: 'abc123',
  domainname: '',
  lang: 'en',
  user: null,
  data: null,
}

const app = createApp(App)
app.mount('#app')