'use client'

import { useTranslations } from 'next-intl'
import { Button, Card, CardBody, Col, Media } from 'reactstrap'

import { APP_META_TITLE } from '@/environment'

import ClockMain from './Clock'

const GreetingCard = () => {
  const t = useTranslations()

  return (
    <Col xxl={4} sm={6} className="box-col-6">
      <Card className="profile-box">
        <CardBody>
          <Media className="media-wrapper justify-content-between">
            <Media body>
              <div className="greeting-user">
                <h4 className="f-w-600">
                  {t('welcomeToCompany', { company: APP_META_TITLE })}
                </h4>
                <p>{t('gettingCardText')}</p>
                <div className="whatsnew-btn">
                  <Button color="" className="btn btn-outline-white" href="#">
                    {t('whatsNew')}
                  </Button>
                </div>
              </div>
            </Media>
            <ClockMain />
            <div className="cartoon">
              <img
                className="img-fluid"
                src={`/assets/images/dashboard/cartoon.svg`}
                alt="vector women with leptop"
              />
            </div>
          </Media>
        </CardBody>
      </Card>
    </Col>
  )
}

export default GreetingCard
