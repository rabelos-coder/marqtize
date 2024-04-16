import { getTranslations } from 'next-intl/server'
import { Col, Container, Row } from 'reactstrap'

import SvgBorder from '@/components/frontend/common/SvgBorder'
import { EMAIL, MOBILE, PHONE } from '@/environment'
import Header from '@/layouts/frontend/landing/Header'
import LandingLayout from '@/layouts/frontend/landing/LandingLayout'
import { Link } from '@/navigation'
import { PageParamsProps } from '@/types/common'
import { concatTitle, generateWhatsAppLink } from '@/utils/helpers'
import { ContactUs } from '@/views/frontend/landing/ContactUs'

export async function generateMetadata({
  params: { locale },
}: PageParamsProps) {
  const t = await getTranslations({ locale })
  const title = concatTitle(t('contact'))

  return {
    title,
  }
}

export default async function ContactPage({
  params: { locale },
}: PageParamsProps) {
  const t = await getTranslations({ locale })

  return (
    <LandingLayout>
      <Header
        title={t('getInTouch')}
        description={t('getInTouchShortDescription')}
      />
      <section className="bg-white py-10">
        <Container className="px-5">
          <Row className="gx-5 justify-content-center">
            <Col lg={8} className="text-center">
              <h2>{t('getInTouchSubtitle')}</h2>
              <p className="lead mb-5">{t('getInTouchDescription')}</p>
            </Col>
          </Row>
          <Row className="gx-5 align-items-center mb-10">
            <Col lg={4} className="text-center mb-5 mb-lg-0">
              <div className="section-preheading">{t('talkToUs')}</div>
              <Link
                href={generateWhatsAppLink(MOBILE, t('whatsAppMessage'))}
                target="_blank"
              >
                {t('startAChat')}!
              </Link>
            </Col>
            <Col lg={4} className="text-center mb-5 mb-lg-0">
              <div className="section-preheading">{t('callUs')}</div>
              <Link href={`tel:${PHONE}`}>{PHONE}</Link>
            </Col>
            <Col lg={4} className="text-center">
              <div className="section-preheading">{t('emailUs')}:</div>
              <Link href={`mailto:${EMAIL}`}>{EMAIL}</Link>
            </Col>
          </Row>
          <ContactUs />
        </Container>
        <SvgBorder className="text-dark" />
      </section>
    </LandingLayout>
  )
}
