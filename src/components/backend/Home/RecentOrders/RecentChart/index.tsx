import dynamic from 'next/dynamic'
import React from 'react'
import { CardBody, Col, Row } from 'reactstrap'

import { RecentOrderChart } from '@/fake/chart'

import OrderDetail from '../OrderDetail'

const ReactApexChart = dynamic(() => import('react-apexcharts'), { ssr: false })

const RecentChart = () => {
  return (
    <CardBody className="pt-0">
      <Row className="recent-wrapper">
        <Col xl={6}>
          <div className="recent-chart">
            {typeof window !== 'undefined' && (
              <ReactApexChart
                type="radialBar"
                height={290}
                options={RecentOrderChart.options}
                series={RecentOrderChart.series}
              />
            )}
          </div>
        </Col>
        <OrderDetail />
      </Row>
    </CardBody>
  )
}

export default RecentChart
