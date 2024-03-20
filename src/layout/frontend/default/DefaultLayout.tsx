'use client'

import '../../../app/assets/scss/landing.scss'

import { useLocale } from 'next-intl'
import { ReCaptchaProvider } from 'next-recaptcha-v3'
import { useEffect } from 'react'
import { ToastContainer } from 'react-toastify'

import { SpinnerBoxed } from '@/components/common/SpinnerBoxed'
import { RECAPTCHA_SITE_KEY } from '@/environment'
import { useAppDispatch, useAppSelector } from '@/hooks'
import { GuestLayout } from '@/layout/common/GuestLayout'
import { AuthProvider } from '@/providers/AuthProvider'
import { setLoading } from '@/store/slices/themeSlice'
import { ChildrenProps } from '@/types/common'

/**
 * Renders the default layout for the application.
 *
 * The Default Layout contains default components and scripts to shared across the application.
 *
 * @param {ChildrenProps} children - The children components to be rendered.
 * @return {JSX.Element} The rendered default layout.
 */
export const DefaultLayout = ({ children }: ChildrenProps) => {
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