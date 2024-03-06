"use client";

import { ReactNode } from "react";

import { Spinner } from "@/components/common/Spinner";
import {
  AclAbility,
  AppAbility,
  buildAbilityFor,
  defaultAcl,
} from "@/configs/ability";
import { useAuth } from "@/hooks";
import { Layout } from "@/layout/backend/Layout";
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
  const { user, isLoggedIn } = useAuth();

  // User is logged in, build ability for the user based on his role
  if (isLoggedIn && user && !ability) ability = buildAbilityFor(user);

  // Check the access of current user and render pages
  if (
    ability &&
    isLoggedIn &&
    user &&
    ability.can(guard.action, guard.subject)
  ) {
    return (
      <AbilityProvider ability={ability}>
        <Layout>{children}</Layout>
      </AbilityProvider>
    );
  } else if (!ability || !isLoggedIn) {
    return <Spinner />;
  }

  // Render Not Authorized component if the current user has limited access
  return <NotAuthorized />;
};
