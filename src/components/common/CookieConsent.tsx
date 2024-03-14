"use client";

import Cookies from "js-cookie";
import { useTranslations } from "next-intl";
import { useEffect, useState } from "react";
import { Button } from "reactstrap";

import { STORAGE_COOKIE_CONSENT } from "@/configs";
import { Link } from "@/navigation";

const localConsent: string | null =
  (Cookies.get(STORAGE_COOKIE_CONSENT) as string) ?? null;

export const CookieConsent = () => {
  const [showConsent, setShowConsent] = useState(true);

  const t = useTranslations("translations");

  const acceptCookie = () => {
    setShowConsent(true);
    Cookies.set(STORAGE_COOKIE_CONSENT, "true");
  };

  useEffect(() => {
    setShowConsent(localConsent === "true");
  }, []);

  if (showConsent) {
    return <></>;
  }

  return (
    <div
      className="alert alert-warning text-center mb-0"
      role="alert"
      style={{
        position: "fixed",
        bottom: 0,
        left: 0,
        width: "100%",
        zIndex: 999,
        borderRadius: 0,
      }}
    >
      &#x1F36A; {t("cookieConsent")}{" "}
      <Link href="/privacy-policy" className="text-primary">
        {t("knowMore")}
      </Link>
      .
      <Button color="primary" size="sm" className="ms-3" onClick={acceptCookie}>
        {t("accept")}
      </Button>
    </div>
  );
};
