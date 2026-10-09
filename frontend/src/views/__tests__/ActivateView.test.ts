import { afterEach, describe, expect, it, vi } from 'vitest'
import { createApp, nextTick } from 'vue'
import { createPinia } from 'pinia'
import { createMemoryHistory, createRouter } from 'vue-router'
import ActivateView from '../ActivateView.vue'
import { locale, t, tn } from '../../lib/i18n'

type Answer = { status?: number; body: unknown }
let unmount = () => {}
afterEach(() => {
  unmount()
  vi.unstubAllGlobals()
  document.body.innerHTML = ''
})

/** The page on /activar?t=tok, the server answering each path with `answers` (a function for the ones that change). */
async function mount(answers: Record<string, Answer | ((body: Record<string, unknown>) => Answer)>) {
  const calls: { path: string; body: Record<string, unknown> }[] = []
  vi.stubGlobal('fetch', async (url: string, init: RequestInit = {}) => {
    const path = url.replace(/^api\//, '').split('?')[0]
    const body = init.body ? JSON.parse(String(init.body)) : {}
    calls.push({ path, body })
    const found = answers[path]
    const answer = typeof found === 'function' ? found(body) : (found ?? { status: 404, body: { error: { code: 'NOT_FOUND', message: 'x' } } })
    return new Response(JSON.stringify(answer.body), { status: answer.status ?? 200 })
  })
  const router = createRouter({
    history: createMemoryHistory(),
    routes: [
      { path: '/activar', component: ActivateView },
      { path: '/tablas', component: { template: '<div />' } },
    ],
  })
  await router.push('/activar?t=tok')
  const host = document.createElement('div')
  document.body.append(host)
  const app = createApp(ActivateView)
  app.use(createPinia()).use(router)
  app.config.globalProperties.$t = t
  app.config.globalProperties.$tn = tn
  locale.value = 'en'
  app.mount(host)
  unmount = () => app.unmount()
  const settle = async () => {
    for (let i = 0; i < 6; i++) await new Promise(r => setTimeout(r, 0))
    await nextTick()
  }
  await settle()
  const submit = async () => {
    host.querySelector('form')!.dispatchEvent(new Event('submit', { cancelable: true }))
    await settle()
  }
  const type = async (input: HTMLInputElement, value: string) => {
    input.value = value
    input.dispatchEvent(new Event('input'))
    await nextTick()
  }
  return { host, calls, submit, type, settle }
}

const lookup = (status: string, extra = {}): Answer => ({
  body: { invitation: { email: 'p•••a@example.org', displayName: 'Paula', role: 'editor', status, codeSent: false, ...extra } },
})

describe('the invitation page', () => {
  it('in the first 7 days: straight to choosing a username and password', async () => {
    const { host } = await mount({ 'invitations/lookup': lookup('pending', { email: 'paula@example.org' }) })
    expect(host.querySelector<HTMLInputElement>('input[autocomplete=username]')!.value).toBe('paula')
    expect(host.textContent).toContain('Create my account')
  })

  it('after 7 days: a code to the email first, then the account with that code', async () => {
    const { host, calls, submit, type } = await mount({
      'invitations/lookup': lookup('expired'),
      'invitations/code': { body: { email: 'p•••a@example.org', minutes: 15 } },
      'invitations/code/check': body =>
        body.code === '123456'
          ? { body: { ok: true, email: 'paula@example.org' } }
          : { status: 400, body: { error: { code: 'CODE_WRONG', message: 'That code is not right' } } },
      'invitations/accept': { status: 201, body: { csrf: 'c', user: { username: 'paula', email: 'paula@example.org' } } },
      bootstrap: { body: { user: { id: 'u', username: 'paula', role: 'editor' }, csrf: 'c', modules: [], settings: {}, sync: {} } },
    })
    expect(host.textContent).toContain('p•••a@example.org')
    expect(host.querySelector('input[autocomplete=username]')).toBeNull()
    expect(host.textContent).toContain('Send me a code')

    await submit()
    expect(calls.some(c => c.path === 'invitations/code')).toBe(true)
    const code = host.querySelector<HTMLInputElement>('input[autocomplete=one-time-code]')!
    expect(code).not.toBeNull()

    await type(code, '000000')
    await submit()
    expect(host.textContent).toContain('That code is not right')
    expect(host.querySelector('input[autocomplete=username]')).toBeNull()

    await type(code, '123456')
    await submit()
    expect(host.querySelector<HTMLInputElement>('input[autocomplete=username]')!.value).toBe('paula')
    const [password, repeat] = host.querySelectorAll<HTMLInputElement>('input[type=password]')
    await type(password, 'secret9')
    await type(repeat, 'secret9')
    await submit()
    expect(calls.find(c => c.path === 'invitations/accept')!.body).toMatchObject({ token: 'tok', code: '123456', username: 'paula' })
    expect(host.textContent).toContain('Your account is ready')
  })

  it('revoked or broken links say to ask a team administrator', async () => {
    const revoked = await mount({ 'invitations/lookup': lookup('revoked') })
    expect(revoked.host.textContent).toMatch(/cancelled.*team administrator/)
    expect(revoked.host.querySelector('input')).toBeNull()
    unmount()
    const broken = await mount({
      'invitations/lookup': { status: 404, body: { error: { code: 'INVITATION_INVALID', message: 'This link is not valid' } } },
    })
    expect(broken.host.textContent).toMatch(/not valid.*team administrator/)
  })
})
