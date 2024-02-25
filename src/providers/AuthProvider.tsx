"use client";

import { AuthContext } from "@/contexts/AuthContext";
import { APP_LANGUAGE, APP_TIMEZONE } from "@/environment";
import { AUTH_LOGIN } from "@/graphql/auth";
import { createApolloClient } from "@/utils/apollo";
import { ComponentProps } from "@/types";
import { AuthContextType, LoginInput, RegisterInput, User } from "@/types/auth";
import { useState } from "react";

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
  const [user, setUser] = useState<User | null>(null);
  const [token, setToken] = useState<string | null>(null);
  const [language, setLanguage] = useState<string | null>(APP_LANGUAGE);
  const [timezone, setTimezone] = useState<string | null>(APP_TIMEZONE);

  /**
   * Function to perform a login using the provided input.
   *
   * @param {LoginInput} input - the input containing login data
   * @return {void}
   */
  const login = async (input: LoginInput): Promise<void> => {
    const client = createApolloClient();

    const { data } = await client.mutate({
      mutation: AUTH_LOGIN,
      variables: { data: input.data },
    });

    if (data?.authLogin?.token) setToken(data.authLogin.token);
    if (data?.authLogin?.user) setUser(data.authLogin.user);
  };

  const register = async (input: RegisterInput): Promise<void> => {
    const client = createApolloClient();

    const { data } = await client.mutate({
      mutation: AUTH_LOGIN,
      variables: { data: input.data },
    });

    if (data?.authLogin?.token) setToken(data.authLogin.token);
    if (data?.authLogin?.user) setUser(data.authLogin.user);
    if (data?.authLogin?.user?.language)
      setLanguage(data.authLogin.user.language);
    if (data?.authLogin?.user?.timezone?.code)
      setTimezone(data.authLogin.user.timezone.code);
  };

  const logout = () => {
    setUser(null);
    setToken(null);
    setLanguage(APP_LANGUAGE);
    setTimezone(APP_TIMEZONE);
  };

  return {
    user,
    token,
    language,
    timezone,
    login,
    logout,
    register,
  };
}
