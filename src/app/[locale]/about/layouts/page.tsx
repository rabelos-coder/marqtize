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
  const title = concatTitle(t('layouts'))

  return {
    title,
  }
}

export default async function LayoutsPage({
  params: { locale },
}: PageParamsProps) {
  const t = await getTranslations({ locale })

  return (
    <LandingLayout>
      <Header title={t('layouts')} description={t('layoutsInfo')} />
      <section className="bg-white py-10">
        <Container className="px-5">
          <Row className="gx-5 justify-content-center">
            <Col className="text-justify">
              <p>{t('speedUpYourWork')}</p>
              <p>
                <strong>{t('advantages')}: </strong>
              </p>
              <List type="unordered">
                <li>
                  <strong>{t('customization')}: </strong>
                  {t('customizationInfo')}
                </li>
                <li>
                  <strong>{t('professionalQuality')}: </strong>
                  {t('professionalQualityInfo')}
                </li>
                <li>
                  <strong>{t('easeOfUse')}: </strong>
                  {t('easeOfUseInfo')}
                </li>
                <li>
                  <strong>{t('variety')}: </strong>
                  {t('varietyInfo')}
                </li>
              </List>
              <p>{t('layoutsInfo2')}</p>
            </Col>
          </Row>
        </Container>
        <SvgBorder className="text-dark" />
      </section>
    </LandingLayout>
  )
}
