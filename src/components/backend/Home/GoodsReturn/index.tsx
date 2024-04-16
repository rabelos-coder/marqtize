import { Col, Row } from 'reactstrap'

import WidgetOne from '@/components/common/WidgetOne'

const GoodsReturn = () => {
  return (
    <Col xxl={'auto'} xl={3} sm={6} className="box-col-6">
      <Row>
        <Col xl={12}>
          <WidgetOne
            data={{
              title: 'Sales return',
              gross: 20,
              total: 7000,
              color: 'warning',
              icon: 'return-box',
            }}
          />
        </Col>
        <Col xl={12}>
          <WidgetOne
            data={{
              title: 'Purchase rate',
              gross: 70,
              total: 5700,
              color: 'success',
              icon: 'rate',
            }}
          />
        </Col>
      </Row>
    </Col>
  )
}

export default GoodsReturn
