import { notFound } from 'next/navigation'
import { getTranslations } from 'next-intl/server'

import { PageParamsProps } from '@/types/common'

export async function generateMetadata({
  params: { locale },
}: PageParamsProps) {
  const t = await getTranslations({ locale })
  const title = t('notFound')

  return {
    title,
  }
}

export default function CatchAllPage() {
  notFound()
}
