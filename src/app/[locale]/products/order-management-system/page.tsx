import { getTranslations } from "next-intl/server";
import { Container } from "reactstrap";

import { SvgBorder } from "@/components/frontend/common/SvgBorder";
import { APP_META_TITLE } from "@/environment";
import { Header } from "@/layout/frontend/landing/Header";
import { LandingLayout } from "@/layout/frontend/landing/LandingLayout";
import { concatTitle } from "@/utils/helpers";

export async function generateMetadata({ params: { locale } }: any) {
  const t = await getTranslations({ locale });
  const title = concatTitle(t("orderManagementSystem.title"));

  return {
    title,
  };
}

export default async function OrderManagementSystemPage({
  params: { locale },
}: any) {
  const t = await getTranslations({ locale });

  return (
    <LandingLayout>
      <Header
        title={t("orderManagementSystem.subtitle")}
        description={t("orderManagementSystem.shortDescription", {
          company: APP_META_TITLE,
        })}
      />
      <section className="bg-white py-10">
        <Container className="px-5 text-justify">
          <h1 className="pb-3">{t("orderManagementSystem.subtitle1")}</h1>
          <div className="pb-3">
            {t.rich("orderManagementSystem.paragraph1", {
              company: APP_META_TITLE,
              b: (chunk) => <strong>{chunk}</strong>,
              ul: (chunk) => <ul>{chunk}</ul>,
              li: (chunk) => <li>{chunk}</li>,
            })}
          </div>
          <h1 className="pb-3">{t("orderManagementSystem.subtitle2")}</h1>
          <div className="pb-3">
            {t.rich("orderManagementSystem.paragraph2", {
              company: APP_META_TITLE,
              b: (chunk) => <strong>{chunk}</strong>,
              ul: (chunk) => <ul>{chunk}</ul>,
              li: (chunk) => <li>{chunk}</li>,
            })}
          </div>
          <h1 className="pb-3">{t("orderManagementSystem.subtitle3")}</h1>
          <div className="pb-3">
            {t("orderManagementSystem.paragraph3", { company: APP_META_TITLE })}
          </div>
        </Container>
        <SvgBorder className="text-dark" />
      </section>
    </LandingLayout>
  );
}
