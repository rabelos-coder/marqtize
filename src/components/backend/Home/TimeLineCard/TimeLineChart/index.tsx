import dynamic from 'next/dynamic'
import { CardBody } from 'reactstrap'

import { TimeLineChartData } from '@/fake/chart'

const ReactApexChart = dynamic(() => import('react-apexcharts'), { ssr: false })

const TimeLineChart = () => {
  return (
    <CardBody className="pt-0">
      <div className="schedule-container">
        {typeof window !== 'undefined' && (
          <ReactApexChart
            height={355}
            type="rangeBar"
            options={TimeLineChartData.options}
            series={TimeLineChartData.series}
          />
        )}
      </div>
    </CardBody>
  )
}

export default TimeLineChart
