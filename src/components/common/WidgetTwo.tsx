import dynamic from 'next/dynamic'
import { Card, CardBody } from 'reactstrap'

import { ChartCardType } from '@/types/dashboard'

const ReactApexChart = dynamic(() => import('react-apexcharts'), { ssr: false })

type WidgetProps = {
  data: ChartCardType
  chartClass?: string
  mainClass?: string
}

const WidgetTwo = ({ data, chartClass, mainClass }: WidgetProps) => {
  return (
    <Card
      className={`widget-1 widget-with-chart ${mainClass ? mainClass : ''}`}
    >
      <CardBody>
        <div>
          <h4 className="mb-1">{data.total}</h4>
          <span className="f-light">{data.title}</span>
        </div>
        <div className={`${chartClass ? chartClass : 'order-chart'}`}>
          {typeof window !== 'undefined' && (
            <ReactApexChart
              type={data.type}
              height={data.chart.options.chart?.height}
              options={data.chart.options}
              series={data.chart.series}
            />
          )}
        </div>
      </CardBody>
    </Card>
  )
}

export default WidgetTwo
