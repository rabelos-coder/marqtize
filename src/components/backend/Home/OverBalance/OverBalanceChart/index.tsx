'use client'

import dynamic from 'next/dynamic'
import { useTranslations } from 'next-intl'
import { CardBody, Col, Row } from 'reactstrap'

import { CurrencyChartData } from '@/fake/chart'

const ReactApexChart = dynamic(() => import('react-apexcharts'), { ssr: false })

const OverBalanceChart = () => {
  const t = useTranslations()

  return (
    <Col xl={9} md={12} sm={7} className="p-0">
      <div className="chart-right">
        <Row>
          <Col xl={12}>
            <CardBody className="p-0">
              <ul className="d-flex balance-data">
                <li>
                  <span className="circle bg-warning"> </span>
                  <span className="f-light ms-1">{t('earnings')}</span>
                </li>
                <li>
                  <span className="circle bg-primary"> </span>
                  <span className="f-light ms-1">{t('expenses')}</span>
                </li>
              </ul>
              <div className="current-sale-container">
                {typeof window !== 'undefined' && (
                  <ReactApexChart
                    type="bar"
                    height={300}
                    options={CurrencyChartData.options}
                    series={CurrencyChartData.series}
                  />
                )}
              </div>
            </CardBody>
          </Col>
        </Row>
      </div>
    </Col>
  )
}

export default OverBalanceChart
