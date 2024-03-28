import { notFound } from 'next/navigation'

import { IS_DEVELOPMENT } from '@/environment'

import BadRequest from '../../bad-request'

export default function BadRequestPage() {
  if (!IS_DEVELOPMENT) notFound()

  return <BadRequest />
}
