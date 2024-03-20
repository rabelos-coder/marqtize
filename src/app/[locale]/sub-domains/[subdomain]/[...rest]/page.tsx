import { notFound } from 'next/navigation'
import { getTranslations } from 'next-intl/server'

export async function generateMetadata({ params: { locale } }: any) {
  const t = await getTranslations({ locale })
  const title = t('notFound')

  return {
    title,
  }
}

export default function CatchAllPage() {
  notFound()
}
