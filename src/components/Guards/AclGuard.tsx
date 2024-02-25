"use client";

import {
  AclAbility,
  AppAbility,
  buildAbilityFor,
  defaultAcl,
} from "@/configs/ability";
import { useSession } from "next-auth/react";
import { ReactNode } from "react";
import { Spinner } from "../Spinner";
import { AbilityProvider } from "@/providers/AbilityProvider";
import { NotAuthorized } from "../NotAuthorized";

type AclGuardProps = {
  children: ReactNode;
  acl?: AclAbility;
};

/**
 * A guard component that checks the user's access based on their abilities and renders pages accordingly.
 *
 * @param {AclGuardProps} props - the properties for the AclGuard component
 * @return {JSX.Element} the rendered JSX based on the user's access and abilities
 */
export const AclGuard = ({ acl, children }: AclGuardProps): JSX.Element => {
  let ability: AppAbility | null = null;

  // Define the acl based on the page
  const guard = acl ?? defaultAcl;

  // Get the user's session
  const { data: session } = useSession();

  // User is logged in, build ability for the user based on his role
  if (session && session?.user && !ability)
    ability = buildAbilityFor(session.user as any);

  // Check the access of current user and render pages
  if (ability && session?.user && ability.can(guard.action, guard.subject)) {
    return <AbilityProvider ability={ability}>{children}</AbilityProvider>;
  } else if (!ability || !session?.user) {
    return <Spinner />;
  }

  // Render Not Authorized component if the current user has limited access
  return <NotAuthorized />;
};
