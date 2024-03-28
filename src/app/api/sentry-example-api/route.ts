import { NextResponse } from 'next/server'

import { SENTRY_EXAMPLE_PAGE_ENABLED } from '@/environment'

export const dynamic = 'force-dynamic'

// A faulty API route to test Sentry's error monitoring
export function GET() {
  if (!SENTRY_EXAMPLE_PAGE_ENABLED)
    return NextResponse.json({ data: 'Sentry Example Page Not Enabled!' })

  throw new Error('Sentry Example API Route Error')

  return NextResponse.json({ data: 'Testing Sentry Error...' })
}
