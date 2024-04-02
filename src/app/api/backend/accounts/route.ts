import { NextRequest, NextResponse } from 'next/server'

import { FIND_MANY_ACCOUNTS } from '@/graphql/account'
import { OrderByEnum } from '@/types/common'
import { apiClient } from '@/utils/apollo'

export async function POST(req: NextRequest) {
  const data = await req.json()

  let variables = {
    where: {
      deletedAt: null,
    },
    orderBy: { systemName: OrderByEnum.ASC },
  }

  if (data) variables = data

  return await apiClient
    .query({
      query: FIND_MANY_ACCOUNTS,
      variables,
      fetchPolicy: 'no-cache',
    })
    .then(({ data }) => NextResponse.json(data?.findManyAccount ?? []))
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
