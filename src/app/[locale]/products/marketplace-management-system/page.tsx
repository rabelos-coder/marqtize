import { getTranslations } from 'next-intl/server'
import { Container } from 'reactstrap'

import SvgBorder from '@/components/frontend/common/SvgBorder'
import { APP_META_TITLE } from '@/environment'
import Header from '@/layouts/frontend/landing/Header'
import LandingLayout from '@/layouts/frontend/landing/LandingLayout'
import { concatTitle } from '@/utils/helpers'

export async function generateMetadata({ params: { locale } }: any) {
  const t = await getTranslations({ locale })
  const title = concatTitle(t('marketplaceManagementSystem.title'))

  return {
    title,
  }
}

export default async function MarketplaceManagementSystemPage({
  params: { locale },
}: any) {
  const t = await getTranslations({ locale })

  return (
    <LandingLayout>
      <Header
        title={t('marketplaceManagementSystem.subtitle')}
        description={t('marketplaceManagementSystem.shortDescription', {
          company: APP_META_TITLE,
        })}
      />
      <section className="bg-white py-10">
        <Container className="px-5 text-justify">
          <h1 className="pb-3">{t('marketplaceManagementSystem.subtitle1')}</h1>
          <div className="pb-3">
            {t.rich('marketplaceManagementSystem.paragraph1', {
              company: APP_META_TITLE,
              b: (chunk) => <strong>{chunk}</strong>,
              ul: (chunk) => <ul>{chunk}</ul>,
              li: (chunk) => <li>{chunk}</li>,
            })}
          </div>
          <h1 className="pb-3">{t('marketplaceManagementSystem.subtitle2')}</h1>
          <div className="pb-3">
            {t.rich('marketplaceManagementSystem.paragraph2', {
              company: APP_META_TITLE,
              b: (chunk) => <strong>{chunk}</strong>,
              ul: (chunk) => <ul>{chunk}</ul>,
              li: (chunk) => <li>{chunk}</li>,
            })}
          </div>
          <h1 className="pb-3">{t('marketplaceManagementSystem.subtitle3')}</h1>
          <div className="pb-3">
            {t('marketplaceManagementSystem.paragraph3', {
              company: APP_META_TITLE,
            })}
          </div>
        </Container>
        <SvgBorder className="text-dark" />
      </section>
    </LandingLayout>
  )
}
