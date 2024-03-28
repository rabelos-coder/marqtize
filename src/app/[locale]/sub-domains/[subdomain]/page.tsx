import { headers } from 'next/headers'
import { getTranslations } from 'next-intl/server'

import { concatTitle } from '@/utils/helpers'
import { DefaultPage } from '@/views/frontend/sub-domains/DefaultPage'

export async function generateMetadata({ params: { locale } }: any) {
  const t = await getTranslations({ locale })
  const title = concatTitle(t('support'))

  return {
    title,
  }
}

export default async function SubDomainPage({ params: { subdomain } }: any) {
  const header = headers()
  const host = header.get('host') ?? ''

  return <DefaultPage host={host} slug={subdomain} />
}
