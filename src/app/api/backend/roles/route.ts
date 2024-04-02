import { NextResponse } from 'next/server'

import { FIND_MANY_ROLES } from '@/graphql/roles'
import { OrderByEnum } from '@/types/common'
import { apiClient } from '@/utils/apollo'

export async function GET() {
  return await apiClient
    .query({
      query: FIND_MANY_ROLES,
      variables: {
        orderBy: { name: OrderByEnum.ASC },
      },
      fetchPolicy: 'no-cache',
    })
    .then(({ data }) => NextResponse.json(data?.findManyRole ?? []))
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
