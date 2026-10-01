<script setup lang="ts">
import { renderMarkdown } from '../../lib/markdown'
import { toolParams, type McpTool } from '../../lib/instructions'

/** The MCP tools exactly as the chats get them: name, description (Markdown) and parameters. */
defineProps<{ tools: McpTool[] }>()
const description = (text: string) => renderMarkdown(text).html
</script>

<template>
  <div class="space-y-6">
    <section v-for="tool in tools" :id="`tool-${tool.name}`" :key="tool.name" class="scroll-mt-4">
      <h2 class="font-mono text-base font-semibold text-brand-800">{{ tool.name }}</h2>
      <!-- renderMarkdown escapes every text. -->
      <div class="md mt-1" v-html="description(tool.description)" />
      <div v-if="toolParams(tool.inputSchema).length" class="md-table mt-2">
        <table>
          <thead>
            <tr>
              <th>{{ $t('Parámetro') }}</th>
              <th>{{ $t('Tipo') }}</th>
              <th>{{ $t('Descripción') }}</th>
            </tr>
          </thead>
          <tbody>
            <tr v-for="p in toolParams(tool.inputSchema)" :key="p.name">
              <td class="font-mono whitespace-nowrap" :style="{ paddingLeft: `${0.5 + p.depth}rem` }">
                {{ p.name }}<span v-if="p.required" class="text-red-700" :title="$t('Obligatorio')">*</span>
              </td>
              <td class="font-mono text-stone-600">{{ p.type }}</td>
              <td>{{ p.description }}</td>
            </tr>
          </tbody>
        </table>
      </div>
      <p v-else class="hint mt-1">{{ $t('Sin parámetros') }}</p>
    </section>
  </div>
</template>
