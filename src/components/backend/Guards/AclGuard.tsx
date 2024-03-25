'use client'

import { ReactNode, useEffect, useState } from 'react'

import { Spinner } from '@/components/common/Spinner'
import {
  AclAbility,
  AppAbility,
  buildAbilityFor,
  defaultAcl,
} from '@/configs/ability'
import { useAuth } from '@/hooks'
import { AuthLayout } from '@/layout/backend/AuthLayout'
import { AbilityProvider } from '@/providers/AbilityProvider'

import { NotAuthorized } from '../NotAuthorized'

type AclGuardProps = {
  children: ReactNode
  acl?: AclAbility
}

/**
 * A guard component that checks the user's access based on their abilities and renders pages accordingly.
 *
 * @param {AclGuardProps} props - the properties for the AclGuard component
 * @return {JSX.Element} the rendered JSX based on the user's access and abilities
 */
export const AclGuard = ({ acl, children }: AclGuardProps): JSX.Element => {
  const [loggedIn, setLoggedIn] = useState(false)
  const [ability, setAbility] = useState<AppAbility | null>(null)

  // Define the acl based on the page
  const guard = acl ?? defaultAcl

  // Get the user's session
  const { jwt, isLoggedIn } = useAuth()

  useEffect(() => {
    setLoggedIn(isLoggedIn)

    // User is logged in, build ability for the user based on his role
    if (isLoggedIn && jwt) setAbility(buildAbilityFor(jwt))
  }, [jwt, isLoggedIn])

  // Check the access of current user and render pages
  if (loggedIn && ability && ability.can(guard.action, guard.subject)) {
    return (
      <AbilityProvider ability={ability}>
        <AuthLayout>{children}</AuthLayout>
      </AbilityProvider>
    )
  } else if (!ability || !loggedIn) {
    return <Spinner />
  }

  // Render Not Authorized component if the current user has limited access
  return <NotAuthorized />
}
