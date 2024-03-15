import { getTranslations } from "next-intl/server";
import { FiArrowRight } from "react-icons/fi";
import { Col, Container, Row } from "reactstrap";

import { SvgBorder } from "@/components/frontend/common/SvgBorder";
import { APP_META_TITLE } from "@/environment";
import { Header } from "@/layout/frontend/landing/Header";
import { LandingLayout } from "@/layout/frontend/landing/LandingLayout";
import { concatTitle } from "@/utils/helpers";

export async function generateMetadata({ params: { locale } }: any) {
  const t = await getTranslations({ locale, namespace: "translations" });
  const title = concatTitle(t("aboutUs"));

  return {
    title,
  };
}

export default async function AboutUsPage({ params: { locale } }: any) {
  const t = await getTranslations({ locale, namespace: "translations" });

  return (
    <LandingLayout>
      <Header
        title={t("whoWeAre")}
        description={t("whoWeAreShortDescription")}
      />
      <section className="bg-white py-10">
        <Container className="px-5">
          <Row className="gx-5 justify-content-center">
            <Col lg={10} className="text-justify">
              <h2 className="mb-4">{t("aboutUs")}</h2>
              <p>{t("whoWeAreDescription1", { company: APP_META_TITLE })}</p>
              <p>{t("whoWeAreDescription2")}</p>
              <p className="mb-0">{t("whoWeAreDescription3")}</p>
              <hr className="my-5" />
              <h4 className="mb-4">
                <div className="icon-stack bg-primary text-white me-2">
                  <FiArrowRight width={24} height={24} />
                </div>
                {t("standOut")}
              </h4>
              <p>{t("standOutDescription1")}</p>
              <p>{t("standOutDescription2", { company: APP_META_TITLE })}</p>
            </Col>
          </Row>
        </Container>
        <SvgBorder className="text-dark" />
      </section>
    </LandingLayout>
  );
}
