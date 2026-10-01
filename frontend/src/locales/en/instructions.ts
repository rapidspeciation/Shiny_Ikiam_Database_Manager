// The AI instructions page (InstructionsView, components/instructions) and its button in the Asistente bar.
export default {
  // AssistantView
  'Instrucciones de la IA': 'AI instructions',
  Instrucciones: 'Instructions',
  'Lo que se le indica al asistente: instrucciones, habilidades y herramientas, con su historial de cambios':
    'What the assistant is told: instructions, skills and tools, with their change history',
  // InstructionsView
  'Instrucciones generales': 'General instructions',
  'Habilidades (skills)': 'Skills',
  'Subagentes (solo Claude)': 'Subagents (Claude only)',
  Herramientas: 'Tools',
  'Apertura de cada espacio de T3': 'Opening of each T3 workspace',
  '{n} herramienta (MCP)': '{n} tool (MCP)',
  '{n} herramientas (MCP)': '{n} tools (MCP)',
  'El resumen que cada chat del asistente (Claude o Codex) lee al empezar. Cada espacio de T3 lo recibe con la apertura (siguiente archivo).':
    'The brief every assistant chat (Claude or Codex) reads when it starts. Each T3 workspace gets it with the opening (next file).',
  'Así recibe cada persona el resumen en su espacio de T3: AGENTS.md (y CLAUDE.md, un enlace a él) con su nombre, el texto de AGENTS.md y las carpetas de su espacio. Aquí con una persona genérica.':
    "How each person's T3 workspace gets the brief: AGENTS.md (and CLAUDE.md, a link to it) with their name, the text of AGENTS.md and their workspace's folders. Shown here with a generic person.",
  'Una habilidad: el asistente la carga cuando la tarea coincide con su descripción. Claude Code la lee de .claude/skills y Codex de .agents/skills.':
    'A skill: the assistant loads it when the task matches its description. Claude Code reads it from .claude/skills and Codex from .agents/skills.',
  'Un archivo de referencia de la habilidad {skill}: el asistente lo lee cuando la habilidad lo indica.':
    'A reference file of the skill {skill}: the assistant reads it when the skill says so.',
  'Un subagente de Claude Code: otro modelo al que el asistente encarga una tarea. Codex no tiene subagentes: hace esa tarea él mismo.':
    'A Claude Code subagent: another model the assistant hands a task to. Codex has no subagents: it does that task itself.',
  'Las herramientas de la app (servidor MCP) que recibe cada chat, tal como las ve el modelo: nombre, descripción y parámetros.':
    "The app's tools (MCP server) every chat gets, exactly as the model sees them: name, description and parameters.",
  'El historial de cambios no está disponible en esta instalación.': 'The change history is not available on this installation.',
  'Archivos de instrucciones': 'Instruction files',
  'No hay ningún archivo con ese nombre.': 'There is no file with that name.',
  'Todos los archivos': 'All files',
  'Cuándo se usa': 'When it is used',
  Modelo: 'Model',
  Esfuerzo: 'Effort',
  Texto: 'Text',
  'Cambios ({n})': 'Changes ({n})',
  'Último cambio: {date}': 'Last changed: {date}',
  'En esta página': 'On this page',
  'Sin cambios registrados todavía.': 'No changes recorded yet.',
  // DiffView
  'Línea {n}': 'Line {n}',
  '{n} línea sin cambios': '{n} unchanged line',
  '{n} líneas sin cambios': '{n} unchanged lines',
  'Archivo nuevo': 'New file',
  'Archivo borrado': 'File deleted',
  'Renombrado desde {path}': 'Renamed from {path}',
  // ToolList
  Parámetro: 'Parameter',
  Tipo: 'Type',
  Descripción: 'Description',
  Obligatorio: 'Required',
  'Sin parámetros': 'No parameters',
}
