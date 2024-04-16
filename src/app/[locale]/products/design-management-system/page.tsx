import { getTranslations } from 'next-intl/server'
import { Container } from 'reactstrap'

import SvgBorder from '@/components/frontend/common/SvgBorder'
import { APP_META_TITLE } from '@/environment'
import Header from '@/layouts/frontend/landing/Header'
import LandingLayout from '@/layouts/frontend/landing/LandingLayout'
import { concatTitle } from '@/utils/helpers'

export async function generateMetadata({ params: { locale } }: any) {
  const t = await getTranslations({ locale })
  const title = concatTitle(t('designManagementSystem.title'))

  return {
    title,
  }
}

export default async function DesignManagementSystemPage({
  params: { locale },
}: any) {
  const t = await getTranslations({ locale })

  return (
    <LandingLayout>
      <Header
        title={t('designManagementSystem.subtitle')}
        description={t('designManagementSystem.shortDescription', {
          company: APP_META_TITLE,
        })}
      />
      <section className="bg-white py-10">
        <Container className="px-5 text-justify">
          <h1 className="pb-3">{t('designManagementSystem.subtitle1')}</h1>
          <div className="pb-3">
            {t.rich('designManagementSystem.paragraph1', {
              company: APP_META_TITLE,
              b: (chunk) => <strong>{chunk}</strong>,
              ul: (chunk) => <ul>{chunk}</ul>,
              li: (chunk) => <li>{chunk}</li>,
            })}
          </div>
          <h1 className="pb-3">{t('designManagementSystem.subtitle2')}</h1>
          <div className="pb-3">
            {t.rich('designManagementSystem.paragraph2', {
              company: APP_META_TITLE,
              b: (chunk) => <strong>{chunk}</strong>,
              ul: (chunk) => <ul>{chunk}</ul>,
              li: (chunk) => <li>{chunk}</li>,
            })}
          </div>
          <h1 className="pb-3">{t('designManagementSystem.subtitle3')}</h1>
          <div className="pb-3">
            {t('designManagementSystem.paragraph3', {
              company: APP_META_TITLE,
            })}
          </div>
        </Container>
        <SvgBorder className="text-dark" />
      </section>
    </LandingLayout>
  )
}
