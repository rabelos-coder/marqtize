import '../../../assets/scss/app.scss'

import { Metadata } from 'next/types'
import { ReCaptchaProvider } from 'next-recaptcha-v3'
import { ToastContainer } from 'react-toastify'

import {
  APP_META_DESCRIPTION,
  APP_META_KEYWORDS,
  APP_META_SLOGAN,
  RECAPTCHA_SITE_KEY,
} from '@/environment'
import GuestLayout from '@/layouts/common/GuestLayout'
import AuthProvider from '@/providers/AuthProvider'
import { concatTitle, icons } from '@/utils/helpers'

export function generateMetadata(): Metadata {
  const title = concatTitle(APP_META_SLOGAN)

  return {
    title,
    description: APP_META_DESCRIPTION,
    keywords: APP_META_KEYWORDS,
    icons,
  }
}

export default function AuthLayout({ children, params: { locale } }: any) {
  return (
    <ReCaptchaProvider
      reCaptchaKey={RECAPTCHA_SITE_KEY}
      language={locale === 'pt-br' ? 'pt-BR' : locale}
    >
      <AuthProvider>
        <GuestLayout>{children}</GuestLayout>
        <ToastContainer />
      </AuthProvider>
    </ReCaptchaProvider>
  )
}
