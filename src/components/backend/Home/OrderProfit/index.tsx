import { Col, Row } from 'reactstrap'

import WidgetTwo from '@/components/common/WidgetTwo'
import { Widgets2ChartData, Widgets2ChartData2 } from '@/fake/chart'

const OrderProfit = () => {
  return (
    <Col xxl="auto" xl={12} sm={6} className="box-col-6">
      <Row>
        <Col xxl={12} xl={6} className="box-col-12">
          <WidgetTwo
            data={{
              title: 'Orders',
              total: '1,80k',
              chart: Widgets2ChartData,
              type: 'bar',
            }}
          />
        </Col>
        <Col xxl={12} xl={6} className="box-col-12">
          <WidgetTwo
            chartClass="profit-chart "
            data={{
              title: 'Profit',
              total: '6,90k',
              chart: Widgets2ChartData2,
              type: 'line',
            }}
          />
        </Col>
      </Row>
    </Col>
  )
}

export default OrderProfit
