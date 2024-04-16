'use client'

import { useTranslations } from 'next-intl'

import ErrorPage from '@/components/common/ErrorPage'

export default function NotFound() {
  const t = useTranslations()

  return (
    <ErrorPage
      title={404}
      description={t.rich('notFoundInfo', { p: (chunk) => <p>{chunk}</p> })}
      titleClassName="font-danger"
      color="danger-gradient"
    />
  )
}
