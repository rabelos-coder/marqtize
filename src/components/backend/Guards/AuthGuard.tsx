"use client";

import { useAuth } from "@/hooks";
import { useRouter } from "@/navigation";
import { ComponentProps } from "@/types";
import { useEffect } from "react";
import { NotAuthorized } from "../NotAuthorized";

/**
 * Asynchronous function to guard access to authenticated routes by checking for a server session.
 *
 * @param {ComponentProps} children - The child components to be rendered within the guarded route.
 * @return {JSX.Element} The original child components to be rendered if the user is authenticated.
 */
export const AuthGuard = ({ children }: ComponentProps): JSX.Element => {
  const { isLoggedIn } = useAuth();

  if (isLoggedIn) {
    return <>{children}</>;
  }

  return <></>;
};
