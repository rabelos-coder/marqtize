import { NextRequest, NextResponse } from 'next/server'

import { FIND_ACCOUNT_BY_SLUG } from '@/graphql/account'
import { apiClient } from '@/utils/apollo'

export async function POST(req: NextRequest) {
  const data = await req.json()

  // sanitize data
  const slug = data?.slug?.trim() ?? null
  const host = data?.host?.trim() ?? null

  if (!slug || !host) {
    return NextResponse.json({ message: 'Invalid request' }, { status: 400 })
  }

  // call the login mutation
  return await apiClient
    .mutate({
      mutation: FIND_ACCOUNT_BY_SLUG,
      variables: { slug, host },
      fetchPolicy: 'no-cache',
    })
    .then(({ data }) =>
      NextResponse.json(data?.findBySlugOrHostAccount ?? null)
    )
    .catch((error) =>
      NextResponse.json({ message: error.message }, { status: 500 })
    )
}
