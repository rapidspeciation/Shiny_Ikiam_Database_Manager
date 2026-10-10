import { use } from 'echarts/core'
import { HeatmapChart } from 'echarts/charts'
import { DataZoomComponent, MarkAreaComponent, MarkLineComponent, VisualMapComponent } from 'echarts/components'

// The chart parts only Monitoreo's report uses (see extras.ts), loaded by EChartCanvas when a chart needs them.
use([DataZoomComponent, HeatmapChart, MarkAreaComponent, MarkLineComponent, VisualMapComponent])
