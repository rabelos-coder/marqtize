import { getTranslations } from "next-intl/server";
import { Container } from "reactstrap";

import { SvgBorder } from "@/components/frontend/common/SvgBorder";
import { Header } from "@/layout/frontend/landing/Header";
import { LandingLayout } from "@/layout/frontend/landing/LandingLayout";
import { concatTitle } from "@/utils/helpers";

export async function generateMetadata({ params: { locale } }: any) {
  const t = await getTranslations({ locale });
  const title = concatTitle(t("documentation.title"));

  return {
    title,
  };
}

export default async function DocumentationPage({ params: { locale } }: any) {
  const t = await getTranslations({ locale });

  return (
    <LandingLayout>
      <Header
        title={t("documentation.title")}
        description={t("documentation.shortDescription")}
      />
      <section className="bg-white py-10">
        <Container className="px-5 text-justify">
          <h1 className="pb-3">{t("documentation.subtitle1")}</h1>
          <p className="mb-3">{t("documentation.paragraph1")}</p>
          <h1 className="pb-3">{t("documentation.subtitle1")}</h1>
          <p className="mb-3">{t("documentation.paragraph1")}</p>
          <h1 className="pb-3">{t("documentation.subtitle2")}</h1>
          <div className="mb-3">
            {t.rich("documentation.paragraph2", {
              b: (chunk) => <strong>{chunk}</strong>,
              ul: (chunk) => <ul>{chunk}</ul>,
              li: (chunk) => <li>{chunk}</li>,
            })}
          </div>
          <h1 className="pb-3">{t("documentation.subtitle3")}</h1>
          <div className="mb-3">
            {t.rich("documentation.paragraph3", {
              b: (chunk) => <strong>{chunk}</strong>,
              ul: (chunk) => <ul>{chunk}</ul>,
              li: (chunk) => <li>{chunk}</li>,
            })}
          </div>
          <h1 className="pb-3">{t("documentation.subtitle4")}</h1>
          <div className="mb-3">
            {t.rich("documentation.paragraph4", {
              b: (chunk) => <strong>{chunk}</strong>,
              ul: (chunk) => <ul>{chunk}</ul>,
              li: (chunk) => <li>{chunk}</li>,
            })}
          </div>
          <h1 className="pb-3">{t("documentation.subtitle5")}</h1>
          <div className="mb-3">
            {t.rich("documentation.paragraph5", {
              b: (chunk) => <strong>{chunk}</strong>,
              ul: (chunk) => <ul>{chunk}</ul>,
              li: (chunk) => <li>{chunk}</li>,
            })}
          </div>
          <h1 className="pb-3">{t("documentation.subtitle6")}</h1>
          <div className="mb-3">
            {t.rich("documentation.paragraph6", {
              b: (chunk) => <strong>{chunk}</strong>,
              ul: (chunk) => <ul>{chunk}</ul>,
              li: (chunk) => <li>{chunk}</li>,
            })}
          </div>
          <h1 className="pb-3">{t("documentation.subtitle7")}</h1>
          <div className="mb-3">
            {t.rich("documentation.paragraph7", {
              ul: (chunk) => <ul>{chunk}</ul>,
              li: (chunk) => <li>{chunk}</li>,
            })}
          </div>
          <h1 className="pb-3">{t("documentation.subtitle8")}</h1>
          <div className="mb-3">
            {t.rich("documentation.paragraph8", {
              ul: (chunk) => <ul>{chunk}</ul>,
              li: (chunk) => <li>{chunk}</li>,
            })}
          </div>
          <p className="mt-5">{t("documentation.paragraph9")}</p>
        </Container>
        <SvgBorder className="text-dark" />
      </section>
    </LandingLayout>
  );
}
