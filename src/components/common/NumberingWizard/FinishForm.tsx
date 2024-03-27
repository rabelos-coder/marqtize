'use client'

import Image from 'next/image'
import { useTranslations } from 'next-intl'
import { Col, Row } from 'reactstrap'

const FinishForm = () => {
  const t = useTranslations()

  return (
    <Row className="g-3 p-4">
      <Col xs={12} className="m-0">
        <div className="successful-form">
          <Image
            width={100}
            height={100}
            className="img-fluid"
            src={`/assets/images/gif/dashboard-8/successful.gif`}
            alt="successful"
          />
          <h6>{t('congratulations')}</h6>
          <p>{t('congratulationsText')}</p>
        </div>
      </Col>
    </Row>
  )
}

export default FinishForm
