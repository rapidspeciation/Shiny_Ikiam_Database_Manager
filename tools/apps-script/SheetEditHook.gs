/**
 * Tells the Ithomiini app which rows someone edited directly in this workbook,
 * so the app shows the change within seconds instead of at its next full read
 * (every 30 minutes once this hook is installed). See README.md.
 *
 * Edits reach the app as { sheet, startRow, numRows }. Inserting, deleting or
 * sorting rows makes the app read that whole sheet again.
 */
const APP_URL = 'https://ithomiini-ikiam.com/api/hooks/sheet-edit';

/** Run once from the editor (it is the first function, so the Run button picks it). */
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
    // The app ignores reports from a workbook other than its own.
    payload: JSON.stringify({ spreadsheetId: SpreadsheetApp.getActive().getId(), events }),
    muteHttpExceptions: true,
  });
  if (response.getResponseCode() !== 202)
    console.warn('The app answered ' + response.getResponseCode() + ': ' + response.getContentText());
  return response;
}

/**
 * The secret shared with the app: the HOOK_SECRET script property, or a
 * HOOK_SECRET constant in a separate Config.gs (tools/apps-script/install.sh writes one).
 */
function hookSecret() {
  return PropertiesService.getScriptProperties().getProperty('HOOK_SECRET') ||
    (typeof HOOK_SECRET === 'string' ? HOOK_SECRET : null);
}

/** Logs "202 {"accepted":0}" when the address and secret are right. */
function testConnection() {
  const response = send([{ sheet: 'connection test', startRow: 1 }]);
  console.log(response.getResponseCode() + ' ' + response.getContentText());
}
