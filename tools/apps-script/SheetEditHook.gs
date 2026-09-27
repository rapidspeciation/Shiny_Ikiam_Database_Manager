/**
 * Tells the Ithomiini app which rows someone edited directly in this workbook,
 * so the app shows the change within seconds instead of at its next full read
 * (every 5 minutes). Install it in the TEST workbook only; see README.md.
 *
 * Edits reach the app as { sheet, startRow, numRows }. Inserting, deleting or
 * sorting rows makes the app read that whole sheet again.
 */
const APP_URL = 'https://tbs-insect-gallery.duckdns.org/ithomiini/api/hooks/sheet-edit';

/**
 * The secret shared with the app: the HOOK_SECRET script property, or a
 * HOOK_SECRET constant in a separate Config.gs (tools/apps-script/install.sh writes one).
 */
function hookSecret() {
  return PropertiesService.getScriptProperties().getProperty('HOOK_SECRET') ||
    (typeof HOOK_SECRET === 'string' ? HOOK_SECRET : null);
}

/** Run once from the editor. */
function setup() {
  if (!hookSecret())
    throw new Error('Add the script property HOOK_SECRET first (Project Settings → Script Properties).');
  const spreadsheet = SpreadsheetApp.getActive();
  for (const trigger of ScriptApp.getProjectTriggers()) ScriptApp.deleteTrigger(trigger);
  ScriptApp.newTrigger('onSheetEdit').forSpreadsheet(spreadsheet).onEdit().create();
  ScriptApp.newTrigger('onSheetChange').forSpreadsheet(spreadsheet).onChange().create();
  testConnection();
}

/** Typing, pasting, deleting cell contents or undo. */
function onSheetEdit(e) {
  const range = e.range;
  send([{ sheet: range.getSheet().getName(), startRow: range.getRow(), numRows: range.getNumRows(), change: 'EDIT' }]);
}

/** Row and sheet structure changes; cell edits already arrive through onSheetEdit. */
function onSheetChange(e) {
  if (e.changeType === 'EDIT' || e.changeType === 'FORMAT') return;
  send([{ sheet: e.source.getActiveSheet().getName(), change: e.changeType }]);
}

function send(events) {
  const secret = hookSecret();
  const response = UrlFetchApp.fetch(APP_URL, {
    method: 'post',
    contentType: 'application/json',
    headers: { 'x-hook-secret': secret },
    payload: JSON.stringify({ events }),
    muteHttpExceptions: true,
  });
  if (response.getResponseCode() !== 202)
    console.warn('The app answered ' + response.getResponseCode() + ': ' + response.getContentText());
  return response;
}

/** Logs "202 {"accepted":0}" when the address and secret are right. */
function testConnection() {
  const response = send([{ sheet: 'connection test', startRow: 1 }]);
  console.log(response.getResponseCode() + ' ' + response.getContentText());
}
