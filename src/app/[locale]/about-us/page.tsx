import { getTranslations } from 'next-intl/server'
import { FiArrowRight } from 'react-icons/fi'
import { Col, Container, Row } from 'reactstrap'

import SvgBorder from '@/components/frontend/common/SvgBorder'
import { APP_META_TITLE } from '@/environment'
import Header from '@/layouts/frontend/landing/Header'
import LandingLayout from '@/layouts/frontend/landing/LandingLayout'
import { concatTitle } from '@/utils/helpers'

export async function generateMetadata({ params: { locale } }: any) {
  const t = await getTranslations({ locale })
  const title = concatTitle(t('aboutUs'))

  return {
    title,
  }
}

export default async function AboutUsPage({ params: { locale } }: any) {
  const t = await getTranslations({ locale })

  return (
    <LandingLayout>
      <Header
        title={t('whoWeAre')}
        description={t('whoWeAreShortDescription')}
      />
      <section className="bg-white py-10">
        <Container className="px-5">
          <Row className="gx-5 justify-content-center">
            <Col lg={10} className="text-justify">
              <h2 className="mb-4">{t('aboutUs')}</h2>
              <p>
                {t.rich('whoWeAreDescription1', {
                  company: APP_META_TITLE,
                  b: (chunk) => <strong>{chunk}</strong>,
                })}
              </p>
              <hr className="my-5" />
              <h4 className="mb-4">
                <div className="icon-stack bg-primary text-white me-2">
                  <FiArrowRight width={24} height={24} />
                </div>
                {t('ourHistory')}
              </h4>
              <p>{t('ourHistoryDescription')}</p>
              <hr className="my-5" />
              <h4 className="mb-4">
                <div className="icon-stack bg-primary text-white me-2">
                  <FiArrowRight width={24} height={24} />
                </div>
                {t('ourMission')}
              </h4>
              <p>{t('ourMissionDescription')}</p>
              <hr className="my-5" />
              <h4 className="mb-4">
                <div className="icon-stack bg-primary text-white me-2">
                  <FiArrowRight width={24} height={24} />
                </div>
                {t('whatSetsUsApart')}
              </h4>
              <p>{t('whatSetsUsApartDescription')}</p>
              <hr className="my-5" />
              <h4 className="mb-4">
                <div className="icon-stack bg-primary text-white me-2">
                  <FiArrowRight width={24} height={24} />
                </div>
                {t('ourCompromise')}
              </h4>
              <p>{t('ourCompromiseDescription')}</p>
              <hr className="my-5" />
              <h4 className="mb-4">
                <div className="icon-stack bg-primary text-white me-2">
                  <FiArrowRight width={24} height={24} />
                </div>
                {t('joinUs')}
              </h4>
              <p>{t('joinUsDescription')}</p>
            </Col>
          </Row>
        </Container>
        <SvgBorder className="text-dark" />
      </section>
    </LandingLayout>
  )
}
