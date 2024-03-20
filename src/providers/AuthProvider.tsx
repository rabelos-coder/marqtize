"use client";

import { useEffect, useState } from "react";

import { AuthContext } from "@/context/AuthContext";
import { APP_LANGUAGE, APP_TIMEZONE } from "@/environment";
import { useAppDispatch, useAppSelector } from "@/hooks";
import { useRouter } from "@/navigation";
import { resetAuth } from "@/store/slices/authSlice";
import { AuthContextType } from "@/types/auth";
import { ChildrenProps } from "@/types/common";
import { User } from "@/types/user";

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

type AuthProviderState = {
  user: User | null;
  token: string | null;
  language: string;
  timezone: string;
  isLoggedIn: boolean;
};

/**
 * Returns an authentication context with user, token, language, timezone, logout, and register functions.
 *
 * @return {AuthContextType} authentication context object
 */
function useProvideAuth(): AuthContextType {
  const [state, setState] = useState<AuthProviderState>({
    user: null,
    token: null,
    language: APP_LANGUAGE,
    timezone: APP_TIMEZONE,
    isLoggedIn: false,
  });

  const router = useRouter();
  const dispatch = useAppDispatch();
  const { user, token, language, timezone, isLoggedIn } = useAppSelector(
    (state) => state.auth
  );

  useEffect(() => {
    setState({
      user,
      token,
      language,
      timezone,
      isLoggedIn,
    });
  }, [user, token, language, timezone, isLoggedIn]);

  const logout = () => {
    dispatch(resetAuth());
    router.push("/auth/login");
  };

  return {
    user: state.user,
    token: state.token,
    language: state.language,
    timezone: state.timezone,
    isLoggedIn: state.isLoggedIn,
    logout,
  };
}
