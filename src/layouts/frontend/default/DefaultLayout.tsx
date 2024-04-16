'use client'

import '../../../app/assets/scss/landing.scss'

import dynamic from 'next/dynamic'
import { useLocale } from 'next-intl'
import { ReCaptchaProvider } from 'next-recaptcha-v3'
import { useEffect } from 'react'
import { ToastContainer } from 'react-toastify'

import { RECAPTCHA_SITE_KEY } from '@/environment'
import { useAppDispatch, useAppSelector } from '@/hooks'
import { setLoading } from '@/store/slices/themeSlice'
import { ChildrenProps } from '@/types/common'

const GuestLayout = dynamic(() => import('@/layouts/common/GuestLayout'))
const AuthProvider = dynamic(() => import('@/providers/AuthProvider'))
const SpinnerBoxed = dynamic(() => import('@/components/common/SpinnerBoxed'))

/**
 * Renders the default layout for the application.
 *
 * The Default Layout contains default components and scripts to shared across the application.
 *
 * @param {ChildrenProps} children - The children components to be rendered.
 * @return {JSX.Element} The rendered default layout.
 */
const DefaultLayout = ({ children }: ChildrenProps) => {
  const locale = useLocale()
  const dispatch = useAppDispatch()
  const { loading } = useAppSelector((state) => state.theme)

  useEffect(() => {
    dispatch(setLoading(false))
  }, [dispatch])

  return (
    <ReCaptchaProvider
      reCaptchaKey={RECAPTCHA_SITE_KEY}
      language={locale === 'pt-br' ? 'pt-BR' : locale}
    >
      {loading ? (
        <SpinnerBoxed color="primary" />
      ) : (
        <GuestLayout>
          <AuthProvider>{children}</AuthProvider>
        </GuestLayout>
      )}
      <ToastContainer position="top-right" closeOnClick />
    </ReCaptchaProvider>
  )
}

export default DefaultLayout
