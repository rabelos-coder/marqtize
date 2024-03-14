import { getTranslations } from "next-intl/server";
import { Col, Container, Row } from "reactstrap";

import { BuiltFor } from "@/components/frontend/landing/BuiltFor";
import { GetInTouch } from "@/components/frontend/landing/GetInTouch";
import { PriceTable } from "@/components/frontend/landing/PricingTable";
import { Services } from "@/components/frontend/landing/Services";
import { Testimonials } from "@/components/frontend/landing/Testimonials";
import { Header } from "@/layout/frontend/landing/Header";
import { LandingLayout } from "@/layout/frontend/landing/LandingLayout";

export default async function HomePage({ params: { locale } }: any) {
  const t = await getTranslations({ locale, namespace: "translations" });

  return (
    <LandingLayout>
      <Header>
        <Container className="px-5">
          <Row className="align-items-center gx-5">
            <Col
              lg={6}
              className="aos-init aos-animate text-white"
              data-aos="fade-up"
            >
              <h1 className="page-header-ui-title">{t("whoWeAre")}</h1>
              <p className="page-header-ui-text mb-5">
                {t("whoWeAreDescription")}
              </p>
            </Col>
            <Col
              lg={6}
              className="d-none d-lg-block aos-init aos-animate"
              data-aos="fade-up"
              data-aos-delay="100"
            >
              <img
                className="img-fluid"
                src="/assets/images/theme/landing/loja-virtual-2.svg"
                alt=""
              />
            </Col>
          </Row>
        </Container>
      </Header>
      <BuiltFor />
      <Services />
      <PriceTable />
      <Testimonials />
      <GetInTouch />
      <hr className="m-0" />
    </LandingLayout>
  );
}
