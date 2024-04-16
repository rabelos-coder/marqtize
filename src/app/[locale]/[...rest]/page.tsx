import { notFound } from 'next/navigation'
import { getTranslations } from 'next-intl/server'

import { PageParamsProps } from '@/types/common'
import { concatTitle } from '@/utils/helpers'

export async function generateMetadata({
  params: { locale },
}: PageParamsProps) {
  const t = await getTranslations({ locale })
  const title = concatTitle(t('notFound'))

  return {
    title,
  }
}

export default function CatchAllPage() {
  notFound()
}
