import { jwtDecode } from 'jwt-decode'
import { NextRequest, NextResponse } from 'next/server'
import createMiddleware from 'next-intl/middleware'

import { STORAGE_AUTH_TOKEN, STORAGE_LOCALE, STORAGE_TIMEZONE } from './configs'
import {
  defaultLocale,
  localeDetection,
  localePrefix,
  locales,
} from './configs/i18n'
import { APP_LANGUAGE, APP_TIMEZONE } from './environment'
import { JWT } from './types/jwt'
import { getValidSubdomain } from './utils/helpers'

const PUBLIC_FILE = /\.(.*)$/

// eslint-disable-next-line @typescript-eslint/ban-ts-comment
// @ts-ignore: Unreachable code error
BigInt.prototype.toJSON = function () {
  const int = Number.parseInt(this.toString())

  return int ?? this.toString()
}

const intlMiddleware = createMiddleware({
  locales,
  localePrefix,
  localeDetection,
  defaultLocale,
})

export default function middleware(request: NextRequest) {
  const url = request.nextUrl.clone()

  if (PUBLIC_FILE.test(url.pathname) || url.pathname.includes('_next')) return

  const token = request.cookies.get(STORAGE_AUTH_TOKEN)?.value
  const locale = request.cookies.get(STORAGE_LOCALE)?.value ?? APP_LANGUAGE
  const timezone = request.cookies.get(STORAGE_TIMEZONE)?.value ?? APP_TIMEZONE

  // set locale
  if (!!request.cookies.get(STORAGE_LOCALE)?.value)
    request.cookies.set(STORAGE_LOCALE, locale)

  // set timezone
  if (!!request.cookies.get(STORAGE_TIMEZONE)?.value)
    request.cookies.set(STORAGE_TIMEZONE, timezone)

  const isApi = url.pathname.split('/').includes('api')
  const isAuth = url.pathname.split('/').includes('auth')
  const isBackend = url.pathname.split('/').includes('backend')

  if (!isApi) {
    if (isBackend && !token && !isAuth) {
      return NextResponse.redirect(
        new URL(`/${locale}/auth/login`, request.url)
      )
    } else if (isBackend && token) {
      try {
        const { exp } = jwtDecode(token) as JWT
        if (exp < Math.round(Date.now() / 1000)) {
          return NextResponse.redirect(
            new URL(`/${locale}/auth/login`, request.url)
          )
        }
      } catch {}
    }

    if (isAuth && token && !isBackend) {
      try {
        const { exp } = jwtDecode(token) as JWT
        if (exp > Math.round(Date.now() / 1000)) {
          return NextResponse.redirect(
            new URL(`/${locale}/backend`, request.url)
          )
        }
      } catch {}
    }

    try {
      const host = request.headers.get('host')
      const subdomain = getValidSubdomain(host)
      if (subdomain && !isApi) {
        url.pathname = `/${locale}/sub-domains/${subdomain}${url.pathname}`

        return NextResponse.rewrite(url)
      }
    } catch {}

    return intlMiddleware(request)
  } else if (isApi && isBackend && !token) {
    return NextResponse.json(
      {
        message: 'Unauthorized',
      },
      { status: 401 }
    )
  }

  return NextResponse.next()
}

export const config = {
  matcher: ['/', '/((?!_next|_vercel|tests|.*\\..*).*)', '/(pt-br|en)/:path*'],
}
