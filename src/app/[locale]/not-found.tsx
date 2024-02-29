import { useTranslations } from "next-intl";

import { ErrorPage } from "@/components/common/ErrorPage";

export default function NotFound() {
  const t = useTranslations("translations");

  return (
    <ErrorPage
      title={404}
      description={t("notFoundInfo")}
      titleClassName="font-danger"
      BtnClassName="btn-danger-gradien"
    />
  );
}
