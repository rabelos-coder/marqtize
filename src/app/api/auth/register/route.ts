import { NextRequest, NextResponse } from 'next/server'
import { getTranslations } from 'next-intl/server'

import { APP_LANGUAGE } from '@/environment'
import { REGISTER } from '@/graphql/auth'
import { UserTypeEnum } from '@/types/enums'
import { apiClient } from '@/utils/apollo'

export async function POST(req: NextRequest) {
  const locale = req.headers.get('locale') ?? APP_LANGUAGE
  const recaptcha = req.headers.get('recaptcha') ?? ''
  const skipRecaptcha = req.headers.get('x-recaptcha-skip') ?? 'false'
  const t = await getTranslations({ locale })
  const data = await req.json()

  // sanitize data
  const name = data?.name?.trim() ?? null
  const systemName = data?.systemName?.trim() ?? null
  const email = data?.email?.trim() ?? null
  const password = data?.password?.trim() ?? null
  const type = UserTypeEnum.CREDENTIAL
  const language = locale

  if (!email || !password || !name || !systemName) {
    return NextResponse.json(
      { message: t('typeFieldsRequired') },
      { status: 400 }
    )
  }

  // call the register mutation
  return await apiClient
    .mutate({
      mutation: REGISTER,
      variables: {
        data: { name, systemName, email, password, type, language },
      },
      fetchPolicy: 'no-cache',
      context: {
        headers: {
          recaptcha,
          'x-recaptcha-skip': skipRecaptcha,
        },
      },
    })
    .then(({ data }) => NextResponse.json(data?.register ?? null))
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
