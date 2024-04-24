import { NextRequest, NextResponse } from 'next/server'
import { getTranslations } from 'next-intl/server'

import { APP_LANGUAGE } from '@/environment'
import { RESET_PASSWORD } from '@/graphql/auth'
import { apiClient } from '@/utils/apollo'

export const dynamic = 'force-dynamic'

export async function POST(req: NextRequest) {
  const recaptcha = req.headers.get('recaptcha') ?? ''
  const skipRecaptcha = req.headers.get('x-recaptcha-skip') ?? 'false'

  const t = await getTranslations({
    locale: req.headers.get('locale') ?? APP_LANGUAGE,
  })

  const data = await req.json()

  // sanitize data
  const email = data?.email?.trim() ?? null
  const resetToken = data?.resetToken?.trim() ?? null
  const password = data?.password?.trim() ?? null

  if (!email || !resetToken || !password) {
    return NextResponse.json(
      { message: t('typeFieldsRequired') },
      { status: 400 }
    )
  }

  // call the login mutation
  return await apiClient
    .mutate({
      mutation: RESET_PASSWORD,
      variables: { data: { email, resetToken, password } },
      fetchPolicy: 'no-cache',
      context: {
        headers: {
          recaptcha,
          'x-recaptcha-skip': skipRecaptcha,
        },
      },
    })
    .then(({ data }) => NextResponse.json(data?.resetPassword ?? null))
    .catch((error) =>
      NextResponse.json(
        {
          name: error.name,
          message: error.message,
          stack: error?.stack ?? null,
        },
        { status: 400 }
      )
    )
}
