"use client";

import "../../../app/assets/scss/landing.scss";

import { useLocale } from "next-intl";
import { ReCaptchaProvider } from "next-recaptcha-v3";
import { useEffect } from "react";
import { ToastContainer } from "react-toastify";

import { SpinnerBoxed } from "@/components/common/SpinnerBoxed";
import { TapTop } from "@/components/common/TapTop";
import { RECAPTCHA_SITE_KEY } from "@/environment";
import { useAppDispatch, useAppSelector } from "@/hooks";
import { GuestLayout } from "@/layout/common/GuestLayout";
import { AuthProvider } from "@/providers/AuthProvider";
import { setLoading } from "@/store/slices/themeSlice";

import { Footer } from "./Footer";
import { NavBar } from "./Navbar/index";

type LandingProps = {
  children: React.ReactNode;
  navbarExpanded?: boolean;
};

export const LandingLayout = ({ navbarExpanded, children }: LandingProps) => {
  const locale = useLocale();
  const dispatch = useAppDispatch();
  const { loading } = useAppSelector((state) => state.theme);

  useEffect(() => {
    // AOS.init();
    if (typeof window !== "undefined") window.scrollTo({ top: 0, left: 0 });
    dispatch(setLoading(false));
  }, [dispatch]);

  return (
    <ReCaptchaProvider
      reCaptchaKey={RECAPTCHA_SITE_KEY}
      language={locale === "pt-br" ? "pt-BR" : locale}
    >
      {loading ? (
        <SpinnerBoxed color="primary" />
      ) : (
        <GuestLayout>
          <AuthProvider>
            <div id="layoutDefault">
              <div id="layoutDefault_content">
                <NavBar navbarExpanded={navbarExpanded} />
                {children}
              </div>
              <Footer />
              <TapTop />
            </div>
          </AuthProvider>
        </GuestLayout>
      )}
      <ToastContainer position="bottom-right" />
    </ReCaptchaProvider>
  );
};
