import axios from 'axios'
import { headers } from 'next/headers'
import { notFound } from 'next/navigation'
import Script from 'next/script'
import { Metadata } from 'next/types'
import { NextIntlClientProvider } from 'next-intl'
import { unstable_setRequestLocale } from 'next-intl/server'
import { Suspense } from 'react'

import { CookieConsent } from '@/components/common/CookieConsent'
import { Spinner } from '@/components/common/Spinner'
import { locales } from '@/configs/i18n'
import { APP_LANGUAGE, APP_WEBSITE, IS_DEVELOPMENT } from '@/environment'
import { ApolloProvider } from '@/providers/ApolloProvider'
import { ReduxProvider } from '@/providers/ReduxProvider'
import { ComponentWithLocaleProps } from '@/types/common'

export async function generateMetadata({
  params: { locale },
}: any): Promise<Metadata> {
  const headersList = headers()

  const referer = headersList?.get('referer') ?? APP_WEBSITE
  const refererURL = new URL(referer)
  const protocol = refererURL.protocol

  const host = headersList?.get('host') ?? ''
  const baseUrl = `${protocol}//${host}`

  return {
    metadataBase: new URL(baseUrl),
    openGraph: {
      locale,
    },
  }
}

export default async function LocaleLayout({
  children,
  params,
}: ComponentWithLocaleProps) {
  const header = headers()
  const host = header.get('host') ?? ''

  let connected = false
  try {
    const { data } = await axios.get(`http://localhost:8097`, {
      timeout: 1500,
    })

    connected = !!data
  } catch {}

  const locale = params?.locale ?? APP_LANGUAGE
  unstable_setRequestLocale(locale)

  let messages
  try {
    messages = (await import(`../../locales/${locale}.json`)).default
  } catch (error) {
    console.error(error)

    return notFound()
  }

  if (!locales.includes(locale)) notFound()

  return (
    <html
      lang={locale === 'pt-br' ? 'pt-BR' : locale}
      suppressHydrationWarning={true}
    >
      <body suppressHydrationWarning={true}>
        <NextIntlClientProvider locale={locale} messages={messages}>
          <ApolloProvider>
            <ReduxProvider host={host}>
              <Suspense fallback={<Spinner />}>
                {children}
                <CookieConsent />
              </Suspense>
            </ReduxProvider>
          </ApolloProvider>
        </NextIntlClientProvider>
        {IS_DEVELOPMENT && connected && <Script src="http://localhost:8097" />}
      </body>
    </html>
  )
}
