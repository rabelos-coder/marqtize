import { NextResponse } from 'next/server'

import { FIND_MANY_CLAIMS } from '@/graphql/claims'
import { apiClient } from '@/utils/apollo'

export async function GET() {
  return await apiClient
    .query({
      query: FIND_MANY_CLAIMS,
    })
    .then(({ data }) => NextResponse.json(data?.findManyClaim ?? []))
    .catch((error) =>
      NextResponse.json({ message: error.message }, { status: 500 })
    )
}
