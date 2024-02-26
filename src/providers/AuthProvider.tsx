"use client";

import { AuthContext } from "@/contexts/AuthContext";
import { APP_LANGUAGE, APP_TIMEZONE } from "@/environment";
import { AUTH_LOGIN } from "@/graphql/auth";
import { createApolloClient } from "@/utils/apollo";
import { ComponentProps } from "@/types";
import { AuthContextType, LoginInput, RegisterInput, User } from "@/types/auth";
import { useCallback, useEffect, useState } from "react";
import { useAppDispatch, useAppSelector } from "@/hooks";
import { authReset, fetchAuth, fetchRegister } from "@/store/slices/authSlice";
import { useRouter } from "@/navigation";

/**
 * AuthProvider component that provides authentication context to its children.
 *
 * @param {ComponentProps} children - The child components to provide authentication context to.
 * @return {JSX.Element} The authentication context provided to the children.
 */
export function AuthProvider({ children }: ComponentProps): JSX.Element {
  const auth = useProvideAuth();

  return <AuthContext.Provider value={auth}>{children}</AuthContext.Provider>;
}

/**
 * Returns an authentication context with user, token, language, timezone, login, logout, and register functions.
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
  const login = useCallback(
    async (input: LoginInput): Promise<void> => {
      dispatch(fetchAuth(input));
    },
    [dispatch]
  );

  /**
   * Function to perform a registration using the provided input.
   *
   * @param {RegisterInput} input - the input containing registration data
   * @return {Promise<void>}
   */
  const register = useCallback(
    async (input: RegisterInput): Promise<void> => {
      dispatch(fetchRegister(input));
    },
    [dispatch]
  );

  const logout = () => {
    dispatch(authReset());
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
    login,
    logout,
    register,
    loading,
    error,
    isLoggedIn,
  };
}
