"use client";

import { useTranslations } from "next-intl";

type SpinnerBoxedProps = {
  type?: "border" | "grow";
  color?:
    | "primary"
    | "secondary"
    | "success"
    | "danger"
    | "warning"
    | "info"
    | "light"
    | "dark"
    | "default";
};

export const SpinnerBoxed = ({ type, color }: SpinnerBoxedProps) => {
  const t = useTranslations("translations");

  return (
    <div
      className="d-flex justify-content-center align-items-center"
      style={{ height: "100vh" }}
    >
      <div
        className={`spinner-${type ?? "border"} text-${color ?? "default"}`}
        role="status"
      >
        <span className="sr-only">{t("loading")}...</span>
      </div>
    </div>
  );
};
