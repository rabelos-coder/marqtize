import { getTranslations } from 'next-intl/server'
import { Col, Container, List, Row } from 'reactstrap'

import SvgBorder from '@/components/frontend/common/SvgBorder'
import Header from '@/layouts/frontend/landing/Header'
import LandingLayout from '@/layouts/frontend/landing/LandingLayout'
import { PageParamsProps } from '@/types/common'
import { concatTitle } from '@/utils/helpers'

export async function generateMetadata({
  params: { locale },
}: PageParamsProps) {
  const t = await getTranslations({ locale })
  const title = concatTitle(t('paymentMethods'))

  return {
    title,
  }
}

export default async function PaymentsPage({
  params: { locale },
}: PageParamsProps) {
  const t = await getTranslations({ locale })

  return (
    <LandingLayout>
      <Header
        title={t('paymentMethods')}
        description={t('paymentMethodsDescription')}
      />
      <section className="bg-white py-10">
        <Container className="px-5">
          <Row className="gx-5 justify-content-center">
            <Col className="text-justify">
              <p>{t('paymentsInfo')}</p>
              <List type="unordered">
                <li>
                  <strong>{t('creditCard')}: </strong>
                  {t('creditCardInfo')}
                </li>
                <li>
                  <strong>{t('debitCard')}: </strong>
                  {t('debitCardInfo')}
                </li>
                <li>
                  <strong>{t('billet')}: </strong>
                  {t('billetInfo')}
                </li>
                <li>
                  <strong>{t('pix')}: </strong>
                  {t('pixInfo')}
                </li>
              </List>
              <p>
                <strong>{t('paymentsInfo2')}</strong>
              </p>
              <p>{t('paymentsInfo3')}</p>
            </Col>
          </Row>
        </Container>
        <SvgBorder className="text-dark" />
      </section>
    </LandingLayout>
  )
}
