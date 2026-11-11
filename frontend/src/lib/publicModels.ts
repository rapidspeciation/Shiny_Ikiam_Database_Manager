/**
 * The offline identifier's models for people without an account: the same files the server sends to the team
 * (server/offline-id.mjs MODELS, byte for byte), published on Hugging Face (3 Nov 2026) and fetched from there in
 * 32 MB ranges, each part checked against its SHA-256 below (the server's parts manifest has the same hashes, so a
 * model downloaded from either source counts as the same model). URLs are pinned to a commit: a new model needs a
 * new list here. Hugging Face answers the Range preflight and its CDN sends `Access-Control-Allow-Origin: *`, which a
 * cross-origin-isolated page (COEP require-corp) can fetch in CORS mode.
 */
import type { FileRole } from './offlineId'

/** `version`: a named version (else its content); `input`: the small model's input size (lib/offlineId.ts studentSizeOf). */
export interface PublicFile {
  name: string
  role: FileRole
  bytes: number
  sha256: string
  url: string
  parts: string[]
  version?: string
  input?: { size: number; values: 'clip' }
  /** Shown with the update in Descargas (the server's list says the same: server/offline-id.mjs STUDENT). */
  notes?: { es?: string; en?: string }
}

// The small model, S_B and the previous one: the list names one (rollback by code: SMALL_L1 here and STUDENT_L1 in
// server/offline-id.mjs; model-versions HANDOFF.md 0b).
// Model S_B (11 Nov 2026), 182 px: the server's built-in small model too (server/offline-id.mjs STUDENT). Commit
// fb2068d5 adds it beside L1.
export const SMALL_SB: PublicFile = { name: "student_SB_int8.onnx", role: "student", bytes: 23380320, sha256: "200343ee87918fb7b8e2d2ff8433d9aa54995c9921ab5b8f942ff3ab25385762", version: "small-SB-20261111", input: { size: 182, values: "clip" },
  url: "https://huggingface.co/fr4nzzch/butterfly-id-small-onnx/resolve/fb2068d595546d1a42e6c0f780afb82500b238c7/student_SB_int8.onnx",
  parts: ["200343ee87918fb7b8e2d2ff8433d9aa54995c9921ab5b8f942ff3ab25385762"],
  notes: { es: "Mejor en fotos nuevas de iNaturalist: acierta la especie en el 81,8\u00a0% frente al 79,4\u00a0% de la versión anterior.", en: "Better on new iNaturalist photos: the species right in 81.8\u00a0% against 79.4\u00a0% for the previous version." } }
// Model L1 (31 Oct 2026), 192 px, at its own commit 748ceb56 (still in the repo).
export const SMALL_L1: PublicFile = { name: "student_L1_int8.onnx", role: "student", bytes: 23565423, sha256: "d6bdc3e5f8721b6ff707b4f46625cc8d8c9af86c0df757f9b2619aed1cc2f5fb", version: "small-L1-20261031", input: { size: 192, values: "clip" },
  url: "https://huggingface.co/fr4nzzch/butterfly-id-small-onnx/resolve/748ceb56dfe2d7caf84f75bbb4b17ae0a986dbed/student_L1_int8.onnx",
  parts: ["d6bdc3e5f8721b6ff707b4f46625cc8d8c9af86c0df757f9b2619aed1cc2f5fb"] }

export const PUBLIC_FILES: PublicFile[] = [
  { name: "wing_seg_s2.onnx", role: "detector", bytes: 11081888, sha256: "32763314aa910988a50c80569b5803075aeae5544a4cdc7e20a6524f5ea9e836",
    url: "https://huggingface.co/fr4nzzch/butterfly-wing-detector-onnx/resolve/793b7de4db0de23778cbd3cccc70fa3f3544497e/wing_seg_s2.onnx",
    parts: ["32763314aa910988a50c80569b5803075aeae5544a4cdc7e20a6524f5ea9e836"] },
  { name: "bioclip25h_visual_int8.onnx", role: "encoder", bytes: 637881455, sha256: "bf27f8e52aa74c295fb725b98fb6bd2fd467f7d4a9e1e2e7f04c610f1245cca1",
    url: "https://huggingface.co/fr4nzzch/bioclip-2.5-vith14-visual-int8-onnx/resolve/988bf8dfda58c7df8150c3d1fe3a425244641ded/bioclip25h_visual_int8.onnx",
    parts: ["9f612177342619a27ae6c23e69cb50da25ab792d11860e5dda6fc35b708171df", "4fc1094aba6be1264d29e6aa2c26c75eadcd5277143aee24ecd3fee62fceca9b", "0f42176f5fcf12bcdf098c414ad4dde11f1238b431bafa4898283ce63fc84a88", "748d384d3d86e94369fe01551c3c4dd0df6a87a3cef94a2ace05e83ba86d52ef", "1d7001ec95601f37936de9afa352698d53bf40801cc05edcdd243043b9ab2c81", "38146d6811a8bb80efa4b5ae2bd9434fb34341d2d7336a3f1305f63aa663cc04", "eda8862d04c9cd94ce0212b6501ebade23a47edc9e99b926d3b6f2feeb45a128", "58a9da120dd02a4addb4e2da35c0eb7231ed6bd35fb60433fc6fe262b3e2d815", "c33ff5f5d59bf45c9a7666cbc3b38d5c134bc8d88acff861db516277d8be12c5", "4ff7bfa2e823587217ca304f24e32de0e1d39f73d2621e448d3c676bbd5b2bf3", "a308e6cbace0acd44c3d643f802b65aa5ffdbdd4867ade72f0bc8084140f8329", "f154747ec814c9325253e1e575f97aaaf0451aa7d43bc76bcc62d5f9eddcf540", "08394c60c8871db29f0fd7c72db5c4ef0df302062c77359b00fbb10fd27ec8a6", "2c71e874f52f6c12ecf3fb96d270d8260820bae89ea39789542700020a67c352", "13217752bfa84ff31cf15f09c1237b3bb505f01d0875b7b6c272eb20948ec8a8", "e05a4da3969aacad186463b32f6692c87da53acce21353d5a81b149f6f943bd8", "fec33ed4f0e3f42e57c1e5d0a0d7aae326819dccb9624c3393cd6c006a030c1f", "894d82bbc18cb9ff2853c5e635de6ff2bd85e608f3b6451a6039011d493a2a4a", "51c94d579fc662f45a666e9a2ede41528817248204c832b98b161abb0310c1da", "06859b7555c752d749d5dfd38dc0de70296ddafc51d4e931c34c6a0e1185e81c"] },
  // Head 6fa6ab78 (5 Nov 2026: the alias recipe retrained on V5N serving crops with label fixes; same 15,347 leaves).
  { name: "head_6fa6ab78.json", role: "head-meta", bytes: 748736, sha256: "9c0845ecc74ad420ea199a168328c47470ea408f5964a3897452c57a571d3735",
    url: "https://huggingface.co/fr4nzzch/butterfly-id-classifier/resolve/4bb06f9080809ca6b3196f49baeea78870a8d1ec/offline/head_6fa6ab78.json",
    parts: ["9c0845ecc74ad420ea199a168328c47470ea408f5964a3897452c57a571d3735"] },
  { name: "head_6fa6ab78_fp16.bin", role: "head", bytes: 31430656, sha256: "d20188e14a19a11eb6f8b82543017ef78314dd89e3e57814d101fe6373112660",
    url: "https://huggingface.co/fr4nzzch/butterfly-id-classifier/resolve/4bb06f9080809ca6b3196f49baeea78870a8d1ec/offline/head_6fa6ab78_fp16.bin",
    parts: ["d20188e14a19a11eb6f8b82543017ef78314dd89e3e57814d101fe6373112660"] },
  SMALL_SB,
]

/** The models' public pages (licence and model card), for «Acerca de y licencias». */
export const MODEL_PAGES = {
  detector: 'https://huggingface.co/fr4nzzch/butterfly-wing-detector-onnx',
  detectorSource: 'https://huggingface.co/fr4nzzch/butterfly-wing-detector-onnx/tree/main/source',
  encoder: 'https://huggingface.co/fr4nzzch/bioclip-2.5-vith14-visual-int8-onnx',
  upstream: 'https://huggingface.co/imageomics/bioclip-2.5-vith14',
  small: 'https://huggingface.co/fr4nzzch/butterfly-id-small-onnx',
  head: 'https://huggingface.co/fr4nzzch/butterfly-id-classifier',
}
