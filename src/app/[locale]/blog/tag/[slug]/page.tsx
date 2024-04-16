import { getTranslations } from 'next-intl/server'

import { redirect } from '@/navigation'
import { ChildrenWithParamsProps, PageParamsProps } from '@/types/common'
import { concatTitle } from '@/utils/helpers'

export async function generateMetadata({
  params: { locale },
}: PageParamsProps) {
  const t = await getTranslations({ locale })

  const title = concatTitle(t('blog.tags.title'))

  return {
    title,
  }
}

export default function BlogTagPage({
  params: { slug },
}: ChildrenWithParamsProps) {
  redirect(`/blog/tag/${slug}/1`)
}
