import { axis, barStyle, base, format } from './chart'
import { monthLabel } from '../../lib/summary'

/**
 * Stacked monthly bars ("YYYY-MM" months) for Inicio. No hover popup (it
 * covered the bars): the month's total is written on top, and "Tabla" lists
 * every value.
 */
export function monthly(months: string[], series: { label: string; color: string; data: number[] }[]) {
  const total = (i: number) => series.reduce((n, s) => n + (s.data[i] || 0), 0)
  return {
    ...base({ legend: series.length > 1 }),
    tooltip: { show: false },
    xAxis: axis('category', months.map(monthLabel)),
    yAxis: { ...axis('value'), minInterval: 1 },
    series: series.map((s, n) => ({
      name: s.label,
      type: 'bar',
      stack: 'total',
      barMaxWidth: 22,
      itemStyle: barStyle(s.color, n === series.length - 1),
      label:
        n === series.length - 1
          ? {
              show: true,
              position: 'top',
              color: '#52514e',
              fontSize: 10,
              formatter: (v: { dataIndex: number }) => (total(v.dataIndex) ? format(total(v.dataIndex)) : ''),
            }
          : { show: false },
      data: s.data,
    })),
  }
}
