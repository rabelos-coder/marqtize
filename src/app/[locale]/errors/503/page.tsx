import { notFound } from 'next/navigation'

import { IS_DEVELOPMENT } from '@/environment'

import ServiceUnavailable from '../../service-unavailable'

export default function MaintenancePage() {
  if (!IS_DEVELOPMENT) notFound()

  return <ServiceUnavailable />
}
