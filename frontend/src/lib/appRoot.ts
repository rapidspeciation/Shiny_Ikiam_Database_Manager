/**
 * The app's folder (where its index.html is), as an absolute URL. The app's pages are relative to it so the app works
 * under any base path; a page in a folder of its own (identificar/) says how to get there with
 * <meta name="app-root" content="../">.
 */
export function appRoot(): string {
  const up = document.querySelector<HTMLMetaElement>('meta[name="app-root"]')?.content ?? './'
  return new URL(up, location.href.split('#')[0]).href
}
