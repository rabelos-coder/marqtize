import { NextRequest, NextResponse } from 'next/server'
import { getTranslations } from 'next-intl/server'

import {
  STORAGE_AUTH_TOKEN,
  STORAGE_LOCALE,
  STORAGE_TIMEZONE,
  STORAGE_USER,
} from '@/configs'
import { APP_LANGUAGE, APP_TIMEZONE } from '@/environment'
import { LOGIN } from '@/graphql/auth'
import { apiClient } from '@/utils/apollo'

export async function POST(req: NextRequest, res: NextResponse) {
  const t = await getTranslations({
    locale: req.headers.get('locale') ?? APP_LANGUAGE,
  })
  const data = await req.json()
  const recaptcha = req.headers.get('recaptcha') ?? ''

  // sanitize input
  const email = data?.email?.trim() ?? null
  const password = data?.password?.trim() ?? null
  const rememberMe = Boolean(data?.rememberMe) ?? false

  if (!email || !password) {
    return NextResponse.json(
      { message: t('typeFieldsRequired') },
      { status: 400 }
    )
  }

  // call the login mutation
  return await apiClient
    .mutate({
      mutation: LOGIN,
      variables: { data: { email, password, rememberMe } },
      fetchPolicy: 'no-cache',
      context: {
        headers: {
          recaptcha,
        },
      },
    })
    .then(({ data }) => {
      if (data?.login) {
        // set  cookies
        res.cookies.set(STORAGE_USER, JSON.stringify(data.login.user))
        res.cookies.set(STORAGE_AUTH_TOKEN, data.login.token)
        res.cookies.set(
          STORAGE_LOCALE,
          data.login.user?.language ?? APP_LANGUAGE
        )
        res.cookies.set(
          STORAGE_TIMEZONE,
          data.login.user?.timezone?.code ?? APP_TIMEZONE
        )

        // delete auth cookies
        res.cookies.delete(STORAGE_USER)
        res.cookies.delete(STORAGE_AUTH_TOKEN)

        // set default cookies
        res.cookies.set(STORAGE_LOCALE, APP_LANGUAGE)
        res.cookies.set(STORAGE_TIMEZONE, APP_TIMEZONE)

        return NextResponse.json(data.login)
      }

      return NextResponse.json(null)
    })
    .catch((error) =>
      NextResponse.json({ message: error.message }, { status: 500 })
    )
}
