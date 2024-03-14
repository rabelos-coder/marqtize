"use client";

import { useTranslations } from "next-intl";
import { Button, Col, Container, Row } from "reactstrap";

import { Link } from "@/navigation";

export const GetInTouch = () => {
  const t = useTranslations("translations");

  return (
    <>
      <section id="get-in-touch" className="bg-light py-10">
        <Container className="px-5 mt-5">
          <Row className="gx-5 align-items-center">
            <Col lg={6} className="aos-init aos-animate" data-aos="fade-up">
              <h4>{t("readyToGetStarted")}</h4>
              <p className="lead mb-5 mb-lg-0 text-gray-500">
                {t("readyToGetStartedDescription")}
              </p>
            </Col>
            <Col
              lg={6}
              className="text-lg-end aos-init aos-animate"
              data-aos="fade-up"
            >
              <Button
                color="primary"
                className="fw-500 me-3 my-2"
                href="#!"
                tag={Link}
              >
                {t("contactSales")}
              </Button>
              <Button
                color="white"
                className="fw-500 my-2 shadow"
                href="/auth/register"
                tag={Link}
              >
                {t("createAccount")}
              </Button>
            </Col>
          </Row>
        </Container>
      </section>
    </>
  );
};
