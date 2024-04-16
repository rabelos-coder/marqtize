import dynamic from 'next/dynamic'
import { CardBody } from 'reactstrap'

import { GrowthChartData } from '@/fake/chart'

const ReactApexChart = dynamic(() => import('react-apexcharts'), { ssr: false })

const FollowerChart = () => {
  return (
    <CardBody className="pt-0">
      <div className="growth-wrapper">
        {typeof window !== 'undefined' && (
          <ReactApexChart
            height={200}
            type="line"
            options={GrowthChartData.options}
            series={GrowthChartData.series}
          />
        )}
      </div>
    </CardBody>
  )
}

export default FollowerChart
