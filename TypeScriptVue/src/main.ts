import { createApp } from 'vue'
import App from './App.vue'

import { empty__Runtime } from './lib/common.ts'

globalThis.runtime = empty__Runtime<string,string>()
globalThis.runtime.session = 'abc123'

const app = createApp(App)
app.mount('#app')