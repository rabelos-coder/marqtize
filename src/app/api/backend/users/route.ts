import { NextRequest, NextResponse } from 'next/server'

import { FIND_MANY_USERS } from '@/graphql/users'
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
      query: FIND_MANY_USERS,
      variables,
      fetchPolicy: 'no-cache',
    })
    .then(({ data }) => NextResponse.json(data?.findManyUser ?? []))
    .catch((error) =>
      NextResponse.json({ message: error.message }, { status: 500 })
    )
}
