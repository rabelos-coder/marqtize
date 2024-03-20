'use client'

import { Inter, Roboto } from 'next/font/google'
import { useTranslations } from 'next-intl'
import { Button } from 'reactstrap'

import { useRouter } from '@/navigation'

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

export default function NotFoundPage() {
  const t = useTranslations()
  const router = useRouter()

  return (
    <div className="d-flex align-items-center justify-content-center vh-100">
      <div className="text-center">
        <h1 className={`display-1 fw-bold ${roboto.className}`}>404</h1>
        <p className={`fs-3 ${roboto.className}`}>
          <span className="text-danger">{t('oops')}</span> {t('notFound')}
        </p>
        <p className={`lead py-3 ${inter.className}`}>{t('notFoundInfo2')}</p>
        <Button
          type="button"
          color="primary"
          className={`${inter.className}`}
          onClick={() => router.back()}
        >
          {t('back')}
        </Button>
      </div>
    </div>
  )
}
