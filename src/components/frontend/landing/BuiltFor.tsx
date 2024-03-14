"use client";

import { useTranslations } from "next-intl";
import { BsCart, BsCode } from "react-icons/bs";
import { FiUser } from "react-icons/fi";
import { Col, Container, Row } from "reactstrap";

import { SvgBorder } from "../common/SvgBorder";

export const BuiltFor = () => {
  const t = useTranslations("translations");

  return (
    <section className="bg-white py-10">
      <Container className="px-5">
        <Row className="gx-5 text-center">
          <Col lg={4} className="mb-5 mb-lg-0">
            <div className="icon-stack icon-stack-xl bg-gradient-primary-to-secondary text-white mb-4">
              <BsCart />
            </div>
            <h3 id="builtFor">{t("increaseYourSales")}</h3>
            <p className="mb-0">{t("increaseYourSalesDescription")}</p>
          </Col>
          <Col lg={4} className="mb-5 mb-lg-0">
            <div className="icon-stack icon-stack-xl bg-gradient-primary-to-secondary text-white mb-4">
              <FiUser />
            </div>
            <h3>{t("improveCustomerExperience")}</h3>
            <p className="mb-0">{t("improveCustomerExperienceDescription")}</p>
          </Col>
          <Col lg={4}>
            <div className="icon-stack icon-stack-xl bg-gradient-primary-to-secondary text-white mb-4">
              <BsCode />
            </div>
            <h3>{t("incomeDiversification")}</h3>
            <p className="mb-0">{t("incomeDiversificationDescription")}</p>
          </Col>
        </Row>
      </Container>
      <SvgBorder className="text-light" />
    </section>
  );
};
