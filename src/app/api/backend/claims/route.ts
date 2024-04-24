import { NextResponse } from 'next/server'

import { FIND_MANY_CLAIMS } from '@/graphql/claims'
import { apiClient } from '@/utils/apollo'

export const dynamic = 'force-dynamic'

export async function GET() {
  return await apiClient
    .query({
      query: FIND_MANY_CLAIMS,
    })
    .then(({ data }) => NextResponse.json(data?.findManyClaim ?? []))
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
