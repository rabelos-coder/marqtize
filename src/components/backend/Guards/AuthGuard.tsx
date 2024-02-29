"use client";

import { useAuth } from "@/hooks";
import { ChildrenProps } from "@/types/common";

/**
 * Asynchronous function to guard access to authenticated routes by checking for a server session.
 *
 * @param {ChildrenProps} children - The child components to be rendered within the guarded route.
 * @return {JSX.Element} The original child components to be rendered if the user is authenticated.
 */
export const AuthGuard = ({ children }: ChildrenProps): JSX.Element => {
  const { isLoggedIn } = useAuth();

  if (isLoggedIn) {
    return <>{children}</>;
  }

  return <></>;
};
