"use client";

import { useLocale, useTranslations } from "next-intl";

import { Link } from "@/navigation";

export default function NotFound() {
  const t = useTranslations();
  const locale = useLocale();

  return (
    <div className="grid h-screen place-content-center px-4">
      <div className="text-center">
        <h1 className="text-gray-400-200 text-9xl font-black sm:text-7xl">
          404
        </h1>
        <p className="text-gray-400-900 text-2xl font-bold tracking-tight sm:text-4xl">
          {t("notFound")}!
        </p>
        <p className="mt-4 text-gray-800">{t("notFoundDescription")}</p>
        <Link
          href="/"
          locale={locale}
          className="mt-6 inline-block rounded bg-indigo-600 px-5 py-3 text-sm font-medium text-white hover:bg-indigo-700 focus:outline-none focus:ring"
        >
          {t("backToHome")}
        </Link>
      </div>
    </div>
  );
}
