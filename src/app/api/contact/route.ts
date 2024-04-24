import { NextRequest, NextResponse } from 'next/server'
import { getTranslations } from 'next-intl/server'

import { APP_LANGUAGE } from '@/environment'
import { CONTACT } from '@/graphql/contact'
import { apiClient } from '@/utils/apollo'

export const dynamic = 'force-dynamic'

export async function POST(req: NextRequest) {
  const locale = req.headers.get('locale') ?? APP_LANGUAGE
  const recaptcha = req.headers.get('recaptcha') ?? ''
  const skipRecaptcha = req.headers.get('x-recaptcha-skip') ?? 'false'
  const t = await getTranslations({ locale })
  const data = await req.json()

  // sanitize data
  const name = data?.name?.trim() ?? null
  const email = data?.email?.trim() ?? null
  const message = data?.message?.trim() ?? null

  if (!email || !email || !message) {
    return NextResponse.json(
      { message: t('typeFieldsRequired') },
      { status: 400 }
    )
  }

  // call the contact mutation
  return await apiClient
    .mutate({
      mutation: CONTACT,
      variables: {
        data: { subject: t('contact'), name, email, message },
      },
      context: {
        headers: {
          recaptcha,
          'x-recaptcha-skip': skipRecaptcha,
        },
      },
    })
    .then(({ data }) => NextResponse.json(data?.sendContact ?? null))
    .catch((error) =>
      NextResponse.json(
        { code: error.code, message: error.message },
        { status: 400 }
      )
    )
}
