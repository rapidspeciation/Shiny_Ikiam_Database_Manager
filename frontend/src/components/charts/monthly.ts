import { axis, barStyle, base, format, tipRow, tipTitle } from './chart'
import { monthLabel } from '../../lib/summary'

/** Stacked monthly bars ("YYYY-MM" months) with a hover summary. */
export function monthly(months: string[], series: { label: string; color: string; data: number[] }[], unit: string) {
  return {
    ...base({ legend: series.length > 1 }),
    tooltip: {
      ...base().tooltip,
      trigger: 'axis',
      axisPointer: { type: 'shadow', shadowStyle: { color: 'rgba(0,0,0,.04)' } },
      formatter: (p: { dataIndex: number }[]) => {
        const i = p[0].dataIndex
        return (
          tipTitle(monthLabel(months[i])) +
          series
            .filter(s => s.data[i])
            .map(s => tipRow(s.color, format(s.data[i]), series.length > 1 ? s.label : unit))
            .join('')
        )
      },
    },
    xAxis: axis('category', months.map(monthLabel)),
    yAxis: { ...axis('value'), minInterval: 1 },
    series: series.map((s, n) => ({
      name: s.label,
      type: 'bar',
      stack: 'total',
      barMaxWidth: 22,
      itemStyle: barStyle(s.color, n === series.length - 1),
      data: s.data,
    })),
  }
}
