'use client'

import { ReactNode } from 'react'

import { useAbility } from '@/hooks'
import { Action } from '@/types/action'
import { Subject } from '@/types/subject'

type CanProps = {
  action: Action
  subject: Subject | string
  children: ReactNode
}

/**
 * Can component to provide ability context to its children.
 *
 * @param {Action | string} action - The ability to check.
 * @param {Subject | string} subject - The subject to check.
 * @param {ReactNode} children - The child components to provide ability context to.
 * @return {JSX.Element} The ability context provided to the children.
 */
const Can = ({ action, subject, children }: CanProps): JSX.Element => {
  const ability = useAbility()

  if (!ability.can(action, subject)) {
    return <></>
  }

  return <>{children}</>
}

export default Can
