import { getTranslations } from "next-intl/server";
import { Container } from "reactstrap";

import { SvgBorder } from "@/components/frontend/common/SvgBorder";
import { Header } from "@/layout/frontend/landing/Header";
import { LandingLayout } from "@/layout/frontend/landing/LandingLayout";
import { concatTitle } from "@/utils/helpers";

export async function generateMetadata({ params: { locale } }: any) {
  const t = await getTranslations({ locale });
  const title = concatTitle(t("changelog.title"));

  return {
    title,
  };
}

export default async function DocumentationPage({ params: { locale } }: any) {
  const t = await getTranslations({ locale });

  return (
    <LandingLayout>
      <Header
        title={t("changelog.title")}
        description={t("changelog.shortDescription")}
      />
      <section className="bg-white py-10">
        <Container className="px-5 text-justify">
          <h1 className="pb-3">{t("changelog.subtitle2")}</h1>
          <div className="mb-3">
            {t.rich("changelog.paragraph1", {
              b: (chunk) => <strong>{chunk}</strong>,
              ul: (chunk) => <ul>{chunk}</ul>,
              li: (chunk) => <li>{chunk}</li>,
            })}
          </div>
          <h1 className="pb-3">{t("changelog.subtitle2")}</h1>
          <div className="mb-3">
            {t.rich("changelog.paragraph2", {
              b: (chunk) => <strong>{chunk}</strong>,
              ul: (chunk) => <ul>{chunk}</ul>,
              li: (chunk) => <li>{chunk}</li>,
            })}
          </div>
          <h1 className="pb-3">{t("changelog.subtitle3")}</h1>
          <div className="mb-3">
            {t.rich("changelog.paragraph3", {
              b: (chunk) => <strong>{chunk}</strong>,
              ul: (chunk) => <ul>{chunk}</ul>,
              li: (chunk) => <li>{chunk}</li>,
            })}
          </div>
          <p className="mt-5">{t("changelog.paragraph4")}</p>
        </Container>
        <SvgBorder className="text-dark" />
      </section>
    </LandingLayout>
  );
}
