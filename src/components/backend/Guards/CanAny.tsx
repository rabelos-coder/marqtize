'use client'

import { ReactNode, useEffect, useState } from 'react'

import { useAbility } from '@/hooks'
import { Action } from '@/types/action'
import { Subject } from '@/types/subject'

type CanAnyAcl = {
  action: Action
  subject: Subject | string
}

type CanAnyProps = {
  acls: CanAnyAcl[]
  children: ReactNode
}

/**
 * Can component to provide ability context to its children.
 *
 * @param {CanAnyAcl} acls - The abilities to check.
 * @param {ReactNode} children - The child components to provide ability context to.
 * @return {JSX.Element} The ability context provided to the children.
 */
export const CanAny = ({ acls, children }: CanAnyProps): JSX.Element => {
  const [can, setCan] = useState(false)
  const ability = useAbility()

  useEffect(() => {
    setCan(acls.some((acl) => ability.can(acl.action, acl.subject)))
  }, [ability, acls])

  if (!can) {
    return <></>
  }

  return <>{children}</>
}
