import { NextResponse } from 'next/server'

import { FIND_MANY_TIMEZONES } from '@/graphql/localization'
import { OrderByEnum } from '@/types/common'
import { apiClient } from '@/utils/apollo'

export async function GET() {
  return await apiClient
    .query({
      query: FIND_MANY_TIMEZONES,
      variables: {
        orderBy: { name: OrderByEnum.ASC },
      },
    })
    .then(({ data }) => NextResponse.json(data?.findManyTimezone ?? []))
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
