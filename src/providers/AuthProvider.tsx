"use client";

import { useEffect, useState } from "react";

import { AuthContext } from "@/context/AuthContext";
import { useAppDispatch, useAppSelector } from "@/hooks";
import { useRouter } from "@/navigation";
import { resetAuth } from "@/store/slices/authSlice";
import { AuthContextType } from "@/types/auth";
import { ChildrenProps } from "@/types/common";

/**
 * AuthProvider component that provides authentication context to its children.
 *
 * @param {ChildrenProps} children - The child components to provide authentication context to.
 * @return {JSX.Element} The authentication context provided to the children.
 */
export function AuthProvider({ children }: ChildrenProps): JSX.Element {
  const auth = useProvideAuth();
  const { theme } = useAppSelector((state) => state.theme);

  useEffect(() => {
    if (theme === "light") {
      document.body.classList.remove("dark-only");
      document.body.classList.add("light-only");
    } else {
      document.body.classList.remove("light-only");
      document.body.classList.add("dark-only");
    }
  }, [theme]);

  return <AuthContext.Provider value={auth}>{children}</AuthContext.Provider>;
}

/**
 * Returns an authentication context with user, token, language, timezone, logout, and register functions.
 *
 * @return {AuthContextType} authentication context object
 */
function useProvideAuth(): AuthContextType {
  const [isLoggedIn, setIsLoggedIn] = useState<boolean>(false);

  const router = useRouter();
  const dispatch = useAppDispatch();
  const { user, token, language, timezone } = useAppSelector(
    (state) => state.auth
  );

  const logout = () => {
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
    logout,
    isLoggedIn,
  };
}
