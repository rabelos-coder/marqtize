import '../assets/css/globals.css'

import { Metadata } from 'next'
import { headers } from 'next/headers'

import {
  APP_GOOGLE_SITE_VERIFICATION,
  APP_META_DESCRIPTION,
  APP_META_KEYWORDS,
  APP_META_SLOGAN,
  APP_META_TITLE,
  APP_WEBSITE,
} from '@/environment'
import { ChildrenProps } from '@/types/common'
import { concatTitle, icons } from '@/utils/helpers'

export async function generateMetadata(): Promise<Metadata> {
  const title = concatTitle(APP_META_SLOGAN)
  const headersList = headers()

  const referer = headersList?.get('referer') ?? APP_WEBSITE
  const refererURL = new URL(referer)
  const protocol = refererURL.protocol

  const host = headersList?.get('host') ?? ''
  const baseUrl = `${protocol}//${host}`
  const url = headersList?.get('next-url')
    ? `${baseUrl}${headersList?.get('next-url')}`
    : 'http://localhost:3000'

  const description = APP_META_DESCRIPTION
  const keywords = APP_META_KEYWORDS

  return {
    title,
    metadataBase: new URL(baseUrl),
    alternates: {
      canonical: '/',
      languages: {
        en: '/en',
        'en-US': '/en',
        pt: '/pt-br',
        'pt-BR': '/pt-br',
      },
    },
    description,
    keywords,
    icons,
    robots: {
      index: true,
      follow: true,
      googleBot: {
        index: true,
        follow: true,
        'max-video-preview': -1,
        'max-image-preview': 'large',
        'max-snippet': -1,
      },
    },
    appleWebApp: true,
    verification: {
      google: APP_GOOGLE_SITE_VERIFICATION,
    },
    openGraph: {
      title,
      locale: 'pt-br',
      type: 'website',
      description,
      url,
      siteName: APP_META_TITLE,
      images: [
        {
          url: '/assets/images/themes/landing/og-image-800x600.png',
          width: 800,
          height: 600,
        },
        {
          url: '/assets/images/themes/landing/og-image-1800x1600.png',
          width: 1800,
          height: 1600,
        },
      ],
    },
  }
}

export default function AppLayout({ children }: ChildrenProps) {
  return children
}
