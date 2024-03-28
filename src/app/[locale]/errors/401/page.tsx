import { notFound } from 'next/navigation'

import { IS_DEVELOPMENT } from '@/environment'

import Unauthorized from '../../unauthorized'

export default function UnauthorizedPage() {
  if (!IS_DEVELOPMENT) notFound()

  return <Unauthorized />
}
