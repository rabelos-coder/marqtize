'use client'

import * as Sentry from '@sentry/nextjs'
import { Inter, Roboto } from 'next/font/google'
import { useTranslations } from 'next-intl'
import { useEffect } from 'react'
import { FaArrowsRotate } from 'react-icons/fa6'
import { Button } from 'reactstrap'

import { IS_DEVELOPMENT } from '@/environment'
import { ErrorPageProps } from '@/types/common'

const roboto = Roboto({
  subsets: ['latin'],
  weight: ['500', '700'],
  display: 'swap',
  preload: true,
})

const inter = Inter({
  subsets: ['latin'],
  weight: ['300', '400', '700'],
  display: 'swap',
  preload: true,
})

export default function ErrorPage({ error, reset }: ErrorPageProps) {
  const t = useTranslations()

  useEffect(() => {
    !IS_DEVELOPMENT ? Sentry.captureException(error) : console.error(error)
  }, [error])

  return (
    <div className="d-flex align-items-center justify-content-center vh-100">
      <div className="text-center">
        <h1 className={`display-1 fw-bold ${roboto.className}`}>500</h1>
        <p className={`fs-3 ${roboto.className}`}>
          <span className="text-danger">{t('oops')}</span>{' '}
          {t('internalServerError')}
        </p>
        <p className={`lead py-3 ${inter.className}`}>
          {t('internalServerErrorInfo')}
        </p>
        <Button
          type="button"
          color="primary"
          className={`${inter.className}`}
          onClick={() => reset()}
        >
          <FaArrowsRotate className="me-2" />
          {t('reload')}
        </Button>
      </div>
    </div>
  )
}
