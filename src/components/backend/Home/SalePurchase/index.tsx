'use client'

import { Col, Row } from 'reactstrap'

import { WidgetOne } from '@/components/common/WidgetOne'

const SalePurchase = () => {
  return (
    <Col xxl="auto" xl={3} sm={6} className="box-col-6">
      <Row>
        <Col xl={12}>
          <WidgetOne
            data={{
              title: 'Purchase',
              gross: 50,
              total: '10,000',
              color: 'secondary',
              icon: 'cart',
            }}
          />
        </Col>
        <Col xl={12}>
          <WidgetOne
            data={{
              title: 'Sales',
              gross: 70,
              total: '4,200',
              color: 'primary',
              icon: 'tag',
            }}
          />
        </Col>
      </Row>
    </Col>
  )
}

export default SalePurchase
