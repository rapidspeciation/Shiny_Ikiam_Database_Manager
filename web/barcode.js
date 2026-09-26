// Code 128 symbol patterns from ZXing, copyright 2008 ZXing authors.
// Apache-2.0; see licenses/ZXing-LICENSE. Renderer authored for this app.
const patterns = ["212222", "222122", "222221", "121223", "121322", "131222", "122213", "122312", "132212", "221213", "221312", "231212", "112232", "122132", "122231", "113222", "123122", "123221", "223211", "221132", "221231", "213212", "223112", "312131", "311222", "321122", "321221", "312212", "322112", "322211", "212123", "212321", "232121", "111323", "131123", "131321", "112313", "132113", "132311", "211313", "231113", "231311", "112133", "112331", "132131", "113123", "113321", "133121", "313121", "211331", "231131", "213113", "213311", "213131", "311123", "311321", "331121", "312113", "312311", "332111", "314111", "221411", "431111", "111224", "111422", "121124", "121421", "141122", "141221", "112214", "112412", "122114", "122411", "142112", "142211", "241211", "221114", "413111", "241112", "134111", "111242", "121142", "121241", "114212", "124112", "124211", "411212", "421112", "421211", "212141", "214121", "412121", "111143", "111341", "131141", "114113", "114311", "411113", "411311", "113141", "114131", "311141", "411131", "211412", "211214", "211232", "2331112"];
export function code128Symbols(text) {
  if (typeof text !== 'string' || !/^[\x20-\x7e]{1,80}$/.test(text)) throw new Error('Barcode requires 1–80 ASCII characters');
  const data = [...text].map(char => char.charCodeAt(0) - 32);
  const checksum = (104 + data.reduce((sum, value, index) => sum + value * (index + 1), 0)) % 103;
  return [104, ...data, checksum, 106];
}
export function code128Svg(text) {
  let x = 10;
  const bars = [];
  for (const symbol of code128Symbols(text)) {
    for (const [index, width] of [...patterns[symbol]].map(Number).entries()) {
      if (index % 2 === 0) bars.push(`<rect x="${x}" y="0" width="${width}" height="64"/>`);
      x += width;
    }
  }
  return `<svg xmlns="http://www.w3.org/2000/svg" role="img" aria-label="Record barcode" viewBox="0 0 ${x + 10} 64" style="display:block;width:100%;height:64px" preserveAspectRatio="none"><rect width="100%" height="100%" fill="white"/><g fill="black">${bars.join('')}</g></svg>`;
}
