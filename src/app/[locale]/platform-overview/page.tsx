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
  const title = concatTitle(t('platformOverview.title'))

  return {
    title,
  }
}

export default async function PlatformOverviewPage({
  params: { locale },
}: PageParamsProps) {
  const t = await getTranslations({ locale })

  return (
    <LandingLayout>
      <Header
        title={t('platformOverview.subtitle')}
        description={t('platformOverview.shortDescription', {
          company: APP_META_TITLE,
        })}
      />
      <section className="bg-white py-10">
        <Container className="px-5 text-justify">
          <h1 className="pb-3">{t('platformOverview.subtitle1')}</h1>
          <div className="pb-3">
            {t.rich('platformOverview.paragraph1', {
              company: APP_META_TITLE,
              b: (chunk) => <strong>{chunk}</strong>,
              ul: (chunk) => <ul>{chunk}</ul>,
              li: (chunk) => <li>{chunk}</li>,
            })}
          </div>
          <h1 className="pb-3">{t('platformOverview.subtitle2')}</h1>
          <div className="pb-3">
            {t.rich('platformOverview.paragraph2', {
              company: APP_META_TITLE,
              b: (chunk) => <strong>{chunk}</strong>,
              ul: (chunk) => <ul>{chunk}</ul>,
              li: (chunk) => <li>{chunk}</li>,
            })}
          </div>
          <h1 className="pb-3">{t('platformOverview.subtitle3')}</h1>
          <div className="pb-3">
            {t('platformOverview.paragraph3', {
              company: APP_META_TITLE,
            })}
          </div>
        </Container>
        <SvgBorder className="text-dark" />
      </section>
    </LandingLayout>
  )
}
