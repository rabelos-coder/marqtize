'use client'

import Link from 'next/link'
import { useTranslations } from 'next-intl'
import { Card, CardBody, Col } from 'reactstrap'

const PreAccountCard = () => {
  const t = useTranslations()

  return (
    <Col xxl={3} md={6} className="box-col-6 col-ed-none wow zoomIn">
      <Card className="purchase-card">
        <img
          className="img-fluid"
          src={`/assets/images/dashboard/purchase.png`}
          alt="vector mens with leptop"
        />
        <CardBody className="pt-3">
          <h6 className="mb-3">
            {t('buy')} <a href="#">{t('proAccount')} </a>
            {t('premiumFeatures')}
          </h6>
          <Link
            className="purchase-btn btn btn-primary btn-hover-effect f-w-500"
            href="https://1.envato.market/3GVzd"
            target="_blank"
          >
            {t('purchaseNow')}
          </Link>
        </CardBody>
      </Card>
    </Col>
  )
}

export default PreAccountCard
