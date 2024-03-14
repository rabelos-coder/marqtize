"use client";

import "../../../app/scss/landing.scss";

// import AOS from "aos";
import { useEffect } from "react";
import { ToastContainer } from "react-toastify";

import { SpinnerBoxed } from "@/components/common/SpinnerBoxed";
import { TapTop } from "@/components/common/TapTop";
import { useAppDispatch, useAppSelector } from "@/hooks";
import { GuestLayout } from "@/layout/common/GuestLayout";
import { AuthProvider } from "@/providers/AuthProvider";
import { setLoading } from "@/store/slices/themeSlice";
import { ChildrenProps } from "@/types/common";

import { Footer } from "./Footer";
import { NavBar } from "./Navbar/index";

export const LandingLayout = ({ children }: ChildrenProps) => {
  const dispatch = useAppDispatch();
  const { loading } = useAppSelector((state) => state.theme);

  useEffect(() => {
    // AOS.init();
    if (typeof window !== "undefined") window.scrollTo({ top: 0, left: 0 });
    dispatch(setLoading(false));
  }, [dispatch]);

  return (
    <>
      {loading ? (
        <SpinnerBoxed color="primary" />
      ) : (
        <GuestLayout>
          <AuthProvider>
            <div id="layoutDefault">
              <div id="layoutDefault_content">
                <NavBar />
                {children}
              </div>
              <Footer />
              <TapTop />
            </div>
          </AuthProvider>
        </GuestLayout>
      )}
      <ToastContainer position="bottom-right" />
    </>
  );
};
