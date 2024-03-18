import { getTranslations } from "next-intl/server";
import { Container } from "reactstrap";

import { SvgBorder } from "@/components/frontend/common/SvgBorder";
import { APP_META_TITLE } from "@/environment";
import { Header } from "@/layout/frontend/landing/Header";
import { LandingLayout } from "@/layout/frontend/landing/LandingLayout";
import { concatTitle } from "@/utils/helpers";

export async function generateMetadata({ params: { locale } }: any) {
  const t = await getTranslations({ locale });
  const title = concatTitle(t("eCommercePlatform.title"));

  return {
    title,
  };
}

export default async function ECommercePlatformPage({
  params: { locale },
}: any) {
  const t = await getTranslations({ locale });

  return (
    <LandingLayout>
      <Header
        title={t("eCommercePlatform.subtitle")}
        description={t("eCommercePlatform.shortDescription", {
          company: APP_META_TITLE,
        })}
      />
      <section className="bg-white py-10">
        <Container className="px-5 text-justify">
          <h1 className="pb-3">{t("eCommercePlatform.subtitle1")}</h1>
          <div className="pb-3">
            {t.rich("eCommercePlatform.paragraph1", {
              company: APP_META_TITLE,
              b: (chunk) => <strong>{chunk}</strong>,
              ul: (chunk) => <ul>{chunk}</ul>,
              li: (chunk) => <li>{chunk}</li>,
            })}
          </div>
          <h1 className="pb-3">{t("eCommercePlatform.subtitle2")}</h1>
          <div className="pb-3">
            {t.rich("eCommercePlatform.paragraph2", {
              company: APP_META_TITLE,
              b: (chunk) => <strong>{chunk}</strong>,
              ul: (chunk) => <ul>{chunk}</ul>,
              li: (chunk) => <li>{chunk}</li>,
            })}
          </div>
          <p className="pt-2">
            {t("eCommercePlatform.paragraph3", { company: APP_META_TITLE })}
          </p>
        </Container>
        <SvgBorder className="text-dark" />
      </section>
    </LandingLayout>
  );
}
