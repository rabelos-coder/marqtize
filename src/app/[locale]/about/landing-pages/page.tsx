import { getTranslations } from "next-intl/server";
import { Col, Container, List, Row } from "reactstrap";

import { SvgBorder } from "@/components/frontend/common/SvgBorder";
import { Header } from "@/layout/frontend/landing/Header";
import { LandingLayout } from "@/layout/frontend/landing/LandingLayout";
import { concatTitle } from "@/utils/helpers";

export async function generateMetadata({ params: { locale } }: any) {
  const t = await getTranslations({ locale, namespace: "translations" });
  const title = concatTitle(t("landingPages"));

  return {
    title,
  };
}

export default async function LandingPagesPage({ params: { locale } }: any) {
  const t = await getTranslations({ locale, namespace: "translations" });

  return (
    <LandingLayout>
      <Header
        title={t("landingPages")}
        description={t("robustPlatformDescription")}
      />
      <section className="bg-white py-10">
        <Container className="px-5">
          <Row className="gx-5 justify-content-center">
            <Col className="text-justify">
              <p>{t("landingPagesInfo")}</p>
              <p>
                <strong>{t("withLandingPageBuilderYouCan")}</strong>
              </p>
              <List type="unordered">
                <li>
                  <strong>{t("increaseYourConversionRates")}: </strong>
                  {t("increaseYourConversionRatesInfo")}
                </li>
                <li>
                  <strong>{t("generateMoreLeads")}: </strong>
                  {t("generateMoreLeadsInfo")}
                </li>
                <li>
                  <strong>{t("growYourSales")}: </strong>
                  {t("growYourSalesInfo")}
                </li>
                <li>
                  <strong>{t("promoteYourBrand")}: </strong>
                  {t("promoteYourBrandInfo")}
                </li>
              </List>
              <p>
                <strong>{t("startCreatingYourLandingPagesToday")}</strong>
              </p>
              <p>{t("tryLandingPagesFree")}</p>
            </Col>
          </Row>
        </Container>
        <SvgBorder className="text-dark" />
      </section>
    </LandingLayout>
  );
}
