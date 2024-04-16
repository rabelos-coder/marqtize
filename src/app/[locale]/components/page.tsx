import { getTranslations } from 'next-intl/server'
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
  const title = concatTitle(t('components.title'))

  return {
    title,
  }
}

export default async function DocumentationPage({
  params: { locale },
}: PageParamsProps) {
  const t = await getTranslations({ locale })

  return (
    <LandingLayout>
      <Header
        title={t('components.title')}
        description={t('components.shortDescription')}
      />
      <section className="bg-white py-10">
        <Container className="px-5 text-justify">
          <h1 className="pb-3">
            {t('components.subtitle1', { company: APP_META_TITLE })}
          </h1>
          <p className="mb-4">{t('components.paragraph1')}</p>
          <h1 className="pb-3">{t('components.subtitle2')}</h1>
          <div className="mb-3">
            {t.rich('components.paragraph2', {
              ul: (chunk) => <ul>{chunk}</ul>,
              li: (chunk) => <li>{chunk}</li>,
            })}
          </div>
          <h1 className="pb-3">{t('components.subtitle3')}</h1>
          <div className="mb-3">
            {t.rich('components.paragraph3', {
              ul: (chunk) => <ul>{chunk}</ul>,
              li: (chunk) => <li>{chunk}</li>,
            })}
          </div>
          <h1 className="pb-3">{t('components.subtitle4')}</h1>
          <div className="mb-3">
            {t.rich('components.paragraph4', {
              ul: (chunk) => <ul>{chunk}</ul>,
              li: (chunk) => <li>{chunk}</li>,
            })}
          </div>
          <h1 className="pb-3">{t('components.subtitle5')}</h1>
          <div className="mb-3">
            {t.rich('components.paragraph5', {
              ul: (chunk) => <ul>{chunk}</ul>,
              li: (chunk) => <li>{chunk}</li>,
            })}
          </div>
          <h1 className="pb-3">{t('components.subtitle6')}</h1>
          <div className="mb-3">
            {t.rich('components.paragraph6', {
              ul: (chunk) => <ul>{chunk}</ul>,
              li: (chunk) => <li>{chunk}</li>,
            })}
          </div>
          <p className="mt-5">{t('components.paragraph7')}</p>
        </Container>
        <SvgBorder className="text-dark" />
      </section>
    </LandingLayout>
  )
}
