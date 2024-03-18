import { getTranslations } from "next-intl/server";
import { Col, Container, List, Row } from "reactstrap";

import { SvgBorder } from "@/components/frontend/common/SvgBorder";
import { Header } from "@/layout/frontend/landing/Header";
import { LandingLayout } from "@/layout/frontend/landing/LandingLayout";
import { concatTitle } from "@/utils/helpers";

export async function generateMetadata({ params: { locale } }: any) {
  const t = await getTranslations({ locale });
  const title = concatTitle(t("robustPlatform"));

  return {
    title,
  };
}

export default async function RobustPlatformPage({ params: { locale } }: any) {
  const t = await getTranslations({ locale });

  return (
    <LandingLayout>
      <Header
        title={t("robustPlatform")}
        description={t("robustPlatformDescription")}
      />
      <section className="bg-white py-10">
        <Container className="px-5">
          <Row className="gx-5 justify-content-center">
            <Col className="text-justify">
              <p>
                <strong>{t("robustPlatformInfo")}</strong>
              </p>
              <p>
                <strong>{t("robustPlatformInfo2")}</strong>
              </p>
              <p>{t("robustPlatformInfo3")}</p>
              <p>
                <strong>{t("advantages")}:</strong>
              </p>
              <List type="unordered">
                <li>
                  <strong>{t("productCatalog")}: </strong>
                  {t("productCatalogInfo")}
                </li>
                <li>
                  <strong>{t("personalizedShoppingExperience")}: </strong>
                  {t("personalizedShoppingExperienceInfo")}
                </li>
                <li>
                  <strong>{t("powerfulMarketingAndPromotions")}: </strong>
                  {t("powerfulMarketingAndPromotionsInfo")}
                </li>
                <li>
                  <strong>{t("efficientOrderManagement")}: </strong>
                  {t("efficientOrderManagementInfo")}
                </li>
                <li>
                  <strong>{t("detailedReporting")}: </strong>
                  {t("detailedReportingInfo")}
                </li>
                <li>
                  <strong>{t("securityAndReliability")}: </strong>
                  {t("securityAndReliabilityInfo")}
                </li>
              </List>
              <p>{t("robustPlatformInfo4")}</p>
              <p>
                <strong>{t("robustPlatformInfo5")}</strong>
              </p>
            </Col>
          </Row>
        </Container>
        <SvgBorder className="text-dark" />
      </section>
    </LandingLayout>
  );
}
