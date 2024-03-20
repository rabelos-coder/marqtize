import Image from 'next/image'
import { useTranslations } from 'next-intl'
import { Col, Container, Row } from 'reactstrap'

import { SvgBorder } from '@/components/frontend/common/SvgBorder'
import { APP_META_TITLE } from '@/environment'
import { Header } from '@/layout/frontend/landing/Header'
import { LandingLayout } from '@/layout/frontend/landing/LandingLayout'

export default function ServiceUnavailable() {
  const t = useTranslations()

  return (
    <LandingLayout>
      <Header
        className="not-found"
        style={{ paddingTop: '6rem', paddingBottom: '6rem' }}
      />
      <section className="bg-white py-10">
        <Container className="px-5">
          <Row className="gx-5 justify-content-center">
            <Col xl={6}>
              <div className="text-center mt-4">
                <Image
                  src="/assets/images/themes/landing/503-error-service-unavailable.svg"
                  width={462}
                  height={287}
                  alt={APP_META_TITLE}
                  className="img-fluid pb-4 text-purple"
                />
                <p className="lead">{t('maintenanceInfo')}</p>
              </div>
            </Col>
          </Row>
        </Container>
        <SvgBorder className="text-dark" />
      </section>
    </LandingLayout>
  )
}
