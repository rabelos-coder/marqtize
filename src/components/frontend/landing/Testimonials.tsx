"use client";

import { useTranslations } from "next-intl";
import { FiActivity, FiUser } from "react-icons/fi";
import { Card, CardBody, Col, Container, Row } from "reactstrap";

import { Link } from "@/navigation";
import { anchorClick } from "@/utils/helpers";

import GoogleIcon from "../../../app/assets/svg/google.svg";
import InstagramIcon from "../../../app/assets/svg/instagram.svg";
import { SvgBorder } from "../common/SvgBorder";

export const Testimonials = () => {
  const t = useTranslations("translations");

  return (
    <section id="testimonials" className="bg-white pt-10">
      <Container className="px-5">
        <Row className="gx-5 mb-10 hidden-lg">
          <Col
            lg={6}
            className="mb-5 mb-lg-0 divider-right aos-init aos-animate"
            data-aos="fade"
          >
            <div className="testimonial p-lg-5">
              <div className="testimonial-brand text-gray-400">
                <GoogleIcon />
              </div>
              <p className="testimonial-quote text-muted">
                "Lorem ipsum dolor, sit amet consectetur adipisicing elit. Ut
                error vel omnis adipisci. Ad nam officiis sapiente dicta
                incidunt harum."
              </p>
              <div className="testimonial-name">Adam Hall</div>
              <div className="testimonial-position">Head of Engineering</div>
            </div>
          </Col>
          <Col
            lg={6}
            className="aos-init aos-animate"
            data-aos="fade"
            data-aos-delay="100"
          >
            <div className="testimonial p-lg-5">
              <div className="testimonial-brand text-gray-400">
                <InstagramIcon />
              </div>
              <p className="testimonial-quote text-muted">
                "Adipisci mollitia nemo magnam iure, temporibus molestiae odit,
                sit harum dolores neque maiores quo eligendi nam corrupti."
              </p>
              <div className="testimonial-name">Lia Peterson</div>
              <div className="testimonial-position">
                Technical Project Manager
              </div>
            </div>
          </Col>
        </Row>
        <Row className="gx-5">
          <Col
            lg={6}
            className="col-lg-6 mb-lg-n10 mb-5 mb-lg-0 z-1 aos-init aos-animate"
            data-aos="fade-right"
          >
            <Card
              className="text-decoration-none h-100 lift"
              href="#about"
              onClick={anchorClick}
              tag={Link}
            >
              <CardBody className="py-5">
                <div className="d-flex align-items-center">
                  <div className="icon-stack icon-stack-xl bg-primary text-white flex-shrink-0">
                    <FiActivity className="feather feather-activity" />
                  </div>
                  <div className="ms-4">
                    <h5 className="text-primary">
                      {t("workSmarterNotHarder")}
                    </h5>
                    <p className="card-text text-gray-600">
                      {t("workSmarterNotHarderDescription")}
                    </p>
                  </div>
                </div>
              </CardBody>
            </Card>
          </Col>
          <Col
            lg={6}
            className="mb-lg-n10 z-1 aos-init aos-animate"
            data-aos="fade-left"
          >
            <Card
              className="text-decoration-none h-100 lift"
              href="#pricing"
              tag={Link}
              onClick={anchorClick}
            >
              <CardBody className="py-5">
                <div className="d-flex align-items-center">
                  <div className="icon-stack icon-stack-xl bg-secondary text-white flex-shrink-0">
                    <FiUser
                      width="24"
                      height="24"
                      className="feather feather-user"
                    />
                  </div>
                  <div className="ms-4">
                    <h5 className="text-secondary">{t("builtForCustomers")}</h5>
                    <p className="card-text text-gray-600">
                      {t("builtForCustomersDescription")}
                    </p>
                  </div>
                </div>
              </CardBody>
            </Card>
          </Col>
        </Row>
      </Container>
      <SvgBorder className="text-light" />
    </section>
  );
};
