'use client'

import { Inter, Roboto } from 'next/font/google'
import { notFound } from 'next/navigation'
import { useTranslations } from 'next-intl'
import { useEffect, useState } from 'react'
import { FiRefreshCcw } from 'react-icons/fi'
import { Button } from 'reactstrap'

import { SpinnerBoxed } from '@/components/common/SpinnerBoxed'
import { useAppDispatch, useAppSelector } from '@/hooks'
import { useRouter } from '@/navigation'
import { fetchAccount } from '@/store/slices/accountSlice'
import { Account } from '@/types/account'

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

type Props = {
  host: string
  slug: string
}

export const DefaultPage = ({ host, slug }: Props) => {
  const [siteAccount, setSiteAccount] = useState<Account>({} as Account)
  const t = useTranslations()
  const router = useRouter()

  const dispatch = useAppDispatch()
  const { account, loading } = useAppSelector((state) => state.account)

  useEffect(() => {
    if (!account) dispatch(fetchAccount({ slug, host }))
    if (account) setSiteAccount(account)
  }, [account, dispatch, host, slug])

  return loading ? (
    <SpinnerBoxed color="primary" />
  ) : account ? (
    <div className="d-flex align-items-center justify-content-center vh-100">
      <div className="text-center">
        <h1 className={`display-5 fw-bold ${roboto.className}`}>
          {siteAccount?.tradingName ?? (
            <span className="text-capitalize">{slug}</span>
          )}
        </h1>
        <p className={`fs-3 ${roboto.className}`}>
          <span className="text-danger">{t('oops')}</span>{' '}
          {t('underDevelopment')}
        </p>
        <p className={`lead py-3 ${inter.className}`}>
          {t('underDevelopmentText')}
        </p>
        <Button
          type="button"
          color="primary"
          className={`${inter.className}`}
          onClick={() => router.refresh()}
        >
          <FiRefreshCcw className="me-2" />
          {t('reload')}
        </Button>
      </div>
    </div>
  ) : (
    notFound()
  )
}
