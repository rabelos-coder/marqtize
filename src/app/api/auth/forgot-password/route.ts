import { NextRequest, NextResponse } from 'next/server'
import { getTranslations } from 'next-intl/server'

import { APP_LANGUAGE } from '@/environment'
import { FORGOT_PASSWORD } from '@/graphql/auth'
import { apiClient } from '@/utils/apollo'

export async function POST(req: NextRequest) {
  const recaptcha = req.headers.get('recaptcha') ?? ''

  const t = await getTranslations({
    locale: req.headers.get('locale') ?? APP_LANGUAGE,
  })

  const data = await req.json()

  // sanitize data
  const email = data?.email?.trim() ?? null

  if (!email) {
    return NextResponse.json(
      { message: t('propertyRequired', { property: t('email') }) },
      { status: 400 }
    )
  }

  const host = req.headers.get('host') ?? 'localhost:3000'
  const protocol = req.headers.get('x-forwarded-proto') ?? 'http'

  // set callbackUrl
  const callbackUrl = `${protocol}://${host}/auth/reset-password`

  // call the login mutation
  return await apiClient
    .mutate({
      mutation: FORGOT_PASSWORD,
      variables: { data: { email, callbackUrl } },
      fetchPolicy: 'no-cache',
      context: {
        headers: {
          recaptcha,
        },
      },
    })
    .then(({ data }) => NextResponse.json(data?.forgotPassword ?? null))
    .catch((error) =>
      NextResponse.json({ message: error.message }, { status: 500 })
    )
}
