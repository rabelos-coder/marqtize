'use client'

import { useTranslations } from 'next-intl'
import { Col } from 'reactstrap'

const OrderDetail = () => {
  const t = useTranslations()

  return (
    <Col xl={6}>
      <ul className="order-content">
        <li>
          <span className="recent-circle bg-primary"> </span>
          <div>
            <span className="f-light f-w-500">{t('cancelled')} </span>
            <h4 className="mt-1 mb-0">
              2,302
              <span className="f-light f-14 f-w-400 ms-1">
                ({t('lastMonth')})
              </span>
            </h4>
          </div>
        </li>
        <li>
          <span className="recent-circle bg-info" />
          <div>
            <span className="f-light f-w-500">{t('delivered')}</span>
            <h4 className="mt-1 mb-0">
              9,302
              <span className="f-light f-14 f-w-400 ms-1">
                ({t('lastMonth')})
              </span>
            </h4>
          </div>
        </li>
      </ul>
    </Col>
  )
}

export default OrderDetail
