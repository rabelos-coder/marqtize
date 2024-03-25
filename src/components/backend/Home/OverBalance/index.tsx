'use client'

import { useTranslations } from 'next-intl'
import React from 'react'
import { Card, CardBody, CardHeader, Col, Row } from 'reactstrap'

import { LightCardData } from '@/fake'

import LightCard from '../LightCard'
import OverBalanceChart from './OverBalanceChart'

const OverBalance = () => {
  const t = useTranslations()

  return (
    <Col xxl={8} lg={12} className="box-col-12">
      <Card>
        <CardHeader className="card-no-border">
          <h5>{t('overallBalance')}</h5>
        </CardHeader>
        <CardBody className="pt-0">
          <Row className="m-0 overall-card">
            <OverBalanceChart />
            <LightCard LightCardData={LightCardData} />
          </Row>
        </CardBody>
      </Card>
    </Col>
  )
}

export default OverBalance
