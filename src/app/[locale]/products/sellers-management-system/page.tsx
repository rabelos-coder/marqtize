import { getTranslations } from 'next-intl/server'
import { Container } from 'reactstrap'

import SvgBorder from '@/components/frontend/common/SvgBorder'
import { APP_META_TITLE } from '@/environment'
import Header from '@/layouts/frontend/landing/Header'
import LandingLayout from '@/layouts/frontend/landing/LandingLayout'
import { concatTitle } from '@/utils/helpers'

export async function generateMetadata({ params: { locale } }: any) {
  const t = await getTranslations({ locale })
  const title = concatTitle(t('sellersManagementSystem.title'))

  return {
    title,
  }
}

export default async function SellersManagementSystemPage({
  params: { locale },
}: any) {
  const t = await getTranslations({ locale })

  return (
    <LandingLayout>
      <Header
        title={t('sellersManagementSystem.subtitle')}
        description={t('sellersManagementSystem.shortDescription', {
          company: APP_META_TITLE,
        })}
      />
      <section className="bg-white py-10">
        <Container className="px-5 text-justify">
          <h1 className="pb-3">{t('sellersManagementSystem.subtitle1')}</h1>
          <div className="pb-3">
            {t.rich('sellersManagementSystem.paragraph1', {
              company: APP_META_TITLE,
              b: (chunk) => <strong>{chunk}</strong>,
              ul: (chunk) => <ul>{chunk}</ul>,
              li: (chunk) => <li>{chunk}</li>,
            })}
          </div>
          <h1 className="pb-3">{t('sellersManagementSystem.subtitle2')}</h1>
          <div className="pb-3">
            {t.rich('sellersManagementSystem.paragraph2', {
              company: APP_META_TITLE,
              b: (chunk) => <strong>{chunk}</strong>,
              ul: (chunk) => <ul>{chunk}</ul>,
              li: (chunk) => <li>{chunk}</li>,
            })}
          </div>
          <h1 className="pb-3">{t('sellersManagementSystem.subtitle3')}</h1>
          <div className="pb-3">
            {t('sellersManagementSystem.paragraph3', {
              company: APP_META_TITLE,
            })}
          </div>
        </Container>
        <SvgBorder className="text-dark" />
      </section>
    </LandingLayout>
  )
}
