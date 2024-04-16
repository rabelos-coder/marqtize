import { getTranslations } from 'next-intl/server'
import { FiArrowRight } from 'react-icons/fi'
import { Container } from 'reactstrap'

import SvgBorder from '@/components/frontend/common/SvgBorder'
import { APP_META_TITLE } from '@/environment'
import Header from '@/layouts/frontend/landing/Header'
import LandingLayout from '@/layouts/frontend/landing/LandingLayout'
import { PageParamsProps } from '@/types/common'
import { concatTitle } from '@/utils/helpers'

export async function generateMetadata({
  params: { locale },
}: PageParamsProps) {
  const t = await getTranslations({ locale })
  const title = concatTitle(t('developersPortal.title'))

  return {
    title,
  }
}

export default async function DevelopersPage({
  params: { locale },
}: PageParamsProps) {
  const t = await getTranslations({ locale })

  return (
    <LandingLayout>
      <Header
        title={t('developersPortal.title')}
        description={t('developersPortal.shortDescription')}
      />
      <section className="bg-white py-10">
        <Container className="px-5 text-justify">
          <h1 className="pb-3">
            {t('developersPortal.welcome', { company: APP_META_TITLE })}
          </h1>
          <p className="lead">{t('developersPortal.paragraph1')}</p>
          <hr className="my-5" />
          <h4 className="mb-4">
            <div className="icon-stack bg-primary text-white me-2">
              <FiArrowRight width={24} height={24} />
            </div>
            {t('developersPortal.subtitle1')}
          </h4>
          <p>{t('developersPortal.paragraph2')}</p>
          <hr className="my-5" />
          <h4 className="mb-4">
            <div className="icon-stack bg-primary text-white me-2">
              <FiArrowRight width={24} height={24} />
            </div>
            {t('developersPortal.subtitle2')}
          </h4>
          <p>{t('developersPortal.paragraph3')}</p>

          <hr className="my-5" />
          <h4 className="mb-4">
            <div className="icon-stack bg-primary text-white me-2">
              <FiArrowRight width={24} height={24} />
            </div>
            {t('developersPortal.subtitle3')}
          </h4>
          <p>{t('developersPortal.paragraph4')}</p>

          <hr className="my-5" />
          <h4 className="mb-4">
            <div className="icon-stack bg-primary text-white me-2">
              <FiArrowRight width={24} height={24} />
            </div>
            {t('developersPortal.subtitle5')}
          </h4>
          <p>{t('developersPortal.paragraph6')}</p>

          <hr className="my-5" />
          <h4 className="mb-4">
            <div className="icon-stack bg-primary text-white me-2">
              <FiArrowRight width={24} height={24} />
            </div>
            {t('developersPortal.subtitle7')}
          </h4>
          <p>{t('developersPortal.paragraph8')}</p>
          <p className="py-3">{t('developersPortal.paragraph9')}</p>
        </Container>
        <SvgBorder className="text-dark" />
      </section>
    </LandingLayout>
  )
}
