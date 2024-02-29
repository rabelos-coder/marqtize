"use client";

import { useCallback, useEffect, useState } from "react";

import { AuthContext } from "@/context/AuthContext";
import { useAppDispatch, useAppSelector } from "@/hooks";
import { useRouter } from "@/navigation";
import { fetchAuth, resetAuth } from "@/store/slices/authSlice";
import { AuthContextType, LoginInput } from "@/types/auth";
import { ChildrenProps } from "@/types/common";

/**
 * AuthProvider component that provides authentication context to its children.
 *
 * @param {ChildrenProps} children - The child components to provide authentication context to.
 * @return {JSX.Element} The authentication context provided to the children.
 */
export function AuthProvider({ children }: ChildrenProps): JSX.Element {
  const auth = useProvideAuth();

  return <AuthContext.Provider value={auth}>{children}</AuthContext.Provider>;
}

/**
 * Returns an authentication context with user, token, language, timezone, signIn, signOut, and register functions.
 *
 * @return {AuthContextType} authentication context object
 */
function useProvideAuth(): AuthContextType {
  const [isLoggedIn, setIsLoggedIn] = useState<boolean>(false);

  const router = useRouter();
  const dispatch = useAppDispatch();
  const { user, token, language, timezone, loading, error } = useAppSelector(
    (state) => state.auth
  );

  /**
   * Function to perform a login using the provided input.
   *
   * @param {LoginInput} input - the input containing login data
   * @return {Promise<void>}
   */
  const signIn = useCallback(
    async (input: LoginInput): Promise<void> => {
      dispatch(fetchAuth(input));
    },
    [dispatch]
  );

  const signOut = () => {
    dispatch(resetAuth());
    router.push("/auth/login");
  };

  useEffect(() => {
    if (token) setIsLoggedIn(true);
  }, [token]);

  return {
    user,
    token,
    language,
    timezone,
    signIn,
    signOut,
    loading,
    error,
    isLoggedIn,
  };
}
