/**
 * A photo picked on the offline identifier page (prueba-offline), read at once.
 *
 * On Android the File from a file input is a content URI whose read grant does not last: once the
 * input is cleared and the models have taken many seconds to load, `file.arrayBuffer()` fails with
 * NotReadableError. So the bytes are read first, in the change handler, before the input is cleared
 * and before any model is loaded; identification gets the bytes and a Blob made from them, never the File.
 */

export interface Photo {
  bytes: ArrayBuffer
  blob: Blob
  name: string
  type: string
  size: number
}

export const PHOTO_UNREADABLE = 'No se pudo leer la foto; si viene de Google Fotos en la nube, descárguela primero o use Tomar foto'

/** "Name: message" of an error (DOMException included), for the page and «Copiar resultado». */
export function errorText(err: unknown): string {
  const e = err as { name?: unknown; message?: unknown } | null
  const name = typeof e?.name === 'string' && e.name ? e.name : ''
  const message = typeof e?.message === 'string' ? e.message : String(err)
  return name && name !== 'Error' ? `${name}: ${message}` : message
}

export async function readPhoto(file: File): Promise<Photo> {
  const bytes = await file.arrayBuffer()
  return { bytes, blob: new Blob([bytes], { type: file.type }), name: file.name, type: file.type, size: bytes.byteLength }
}

/**
 * The file inputs' change handler: read the photo's bytes, then clear the input, then identify.
 * A photo that cannot be read goes to `unreadable` (with the error) and nothing is identified.
 */
export async function takePhoto(
  input: { files: FileList | ArrayLike<File> | null; value: string },
  identify: (photo: Photo) => unknown,
  unreadable: (err: unknown) => void,
): Promise<void> {
  const file = input.files?.[0]
  if (!file) {
    input.value = ''
    return
  }
  let photo: Photo
  try {
    photo = await readPhoto(file)
  } catch (err) {
    input.value = ''
    unreadable(err)
    return
  }
  input.value = ''
  await identify(photo)
}
