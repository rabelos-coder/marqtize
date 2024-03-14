"use client";

import { useTranslations } from "next-intl";
import { FaCheckCircle, FaCircle, FaQuestionCircle } from "react-icons/fa";
import {
  Badge,
  Button,
  Card,
  CardBody,
  Col,
  Container,
  List,
  Row,
} from "reactstrap";

import { Link } from "@/navigation";

import { SvgBorder } from "../common/SvgBorder";

export const PriceTable = () => {
  const t = useTranslations("translations");

  return (
    <>
      <section id="pricing" className="bg-light pt-10">
        <Container className="px-5">
          <div className="text-center mb-5">
            <h2>{t("pricingTable")}</h2>
            <p className="lead">{t("pricingTableDescription")}</p>
          </div>
          <Row className="gx-5 z-1">
            <Col
              lg={4}
              className="mb-5 mb-lg-n10 aos-init aos-animate"
              data-aos="fade-up"
              data-aos-delay="100"
            >
              <Card className="pricing h-100">
                <CardBody className="p-5">
                  <div className="text-center">
                    <div className="badge bg-light text-dark rounded-pill badge-marketing badge-sm">
                      {t("free")}
                    </div>
                    <div className="pricing-price">
                      <sup>{t("usd")}</sup>0
                      <span className="pricing-price-period">/{t("mo")}</span>
                    </div>
                  </div>
                  <List type="unstyled" className="fa-ul pricing-list">
                    <li className="pricing-list-item">
                      <span className="fa-li me-2">
                        <FaCheckCircle className="text-teal" />
                      </span>
                      <span className="text-dark">1 {t("user")}</span>
                    </li>
                    <li className="pricing-list-item">
                      <span className="fa-li me-2">
                        <FaCheckCircle className="text-teal" />
                      </span>
                      <span className="text-dark">
                        {t("upToCountProducts", { count: 10 })}
                      </span>
                    </li>
                    <li className="pricing-list-item">
                      <span className="fa-li me-2">
                        <FaCheckCircle className="text-teal" />
                      </span>
                      <span className="text-dark">{t("communitySupport")}</span>
                    </li>
                    <li className="pricing-list-item">
                      <span className="fa-li me-2">
                        <FaCircle className="text-gray-200" />
                      </span>
                      {t("styleCustomizer")}
                    </li>
                    <li className="pricing-list-item">
                      <span className="fa-li me-2">
                        <FaCircle className="text-gray-200" />
                      </span>
                      {t("customComponents")}
                    </li>
                    <li className="pricing-list-item">
                      <span className="fa-li me-2">
                        <FaCircle className="text-gray-200" />
                      </span>
                      {t("expandedUtilities")}
                    </li>
                    <li className="pricing-list-item">
                      <span className="fa-li me-2">
                        <FaCircle className="text-gray-200" />
                      </span>
                      {t("thirdPartyIntegration")}
                    </li>
                    <li className="pricing-list-item">
                      <span className="fa-li me-2">
                        <FaCircle className="text-gray-200" />
                      </span>
                      {t("layoutOptions")}
                    </li>
                  </List>
                </CardBody>
              </Card>
            </Col>
            <Col
              lg={4}
              className="mb-5 mb-lg-n10 aos-init aos-animate"
              data-aos="fade-up"
            >
              <Card className="pricing h-100">
                <CardBody className="p-5">
                  <div className="text-center">
                    <div className="badge bg-primary-soft rounded-pill badge-marketing badge-sm text-primary">
                      {t("standard")}
                    </div>
                    <div className="pricing-price">
                      <sup>{t("usd")}</sup>
                      149
                      <span className="pricing-price-period">/{t("mo")}</span>
                    </div>
                  </div>
                  <List type="unstyled" className="fa-ul pricing-list">
                    <li className="pricing-list-item">
                      <span className="fa-li me-2">
                        <FaCheckCircle className="text-teal" />
                      </span>
                      <span className="text-dark">
                        {t("upToCountUsers", { count: 10 })}
                      </span>
                    </li>
                    <li className="pricing-list-item">
                      <span className="fa-li me-2">
                        <FaCheckCircle className="text-teal" />
                      </span>
                      <span className="text-dark">
                        {t("upToCountProducts", { count: 100 })}
                      </span>
                    </li>
                    <li className="pricing-list-item">
                      <span className="fa-li me-2">
                        <FaCheckCircle className="text-teal" />
                      </span>
                      <span className="text-dark">
                        {t("ticketAndLiveChatSupport")}
                      </span>
                    </li>
                    <li className="pricing-list-item">
                      <span className="fa-li me-2">
                        <FaCheckCircle className="text-teal" />
                      </span>
                      <span className="text-dark">{t("styleCustomizer")}</span>
                    </li>
                    <li className="pricing-list-item">
                      <span className="fa-li me-2">
                        <FaCheckCircle className="text-teal" />
                      </span>
                      <span className="text-dark">{t("customComponents")}</span>
                    </li>
                    <li className="pricing-list-item">
                      <span className="fa-li me-2">
                        <FaCheckCircle className="text-teal" />
                      </span>
                      <span className="text-dark">
                        {t("expandedUtilities")}
                      </span>
                    </li>
                    <li className="pricing-list-item">
                      <span className="fa-li me-2">
                        <FaCheckCircle className="text-teal" />
                      </span>
                      <span className="text-dark">
                        {t("thirdPartyIntegration")}
                      </span>
                    </li>
                    <li className="pricing-list-item">
                      <span className="fa-li me-2">
                        <FaCheckCircle className="text-teal" />
                      </span>
                      <span className="text-dark">{t("layoutOptions")}</span>
                    </li>
                  </List>
                </CardBody>
              </Card>
            </Col>
            <Col
              lg={4}
              className="mb-lg-n10 aos-init aos-animate"
              data-aos="fade-up"
              data-aos-delay="100"
            >
              <Card className="pricing h-100">
                <CardBody className="p-5">
                  <div className="text-center">
                    <Badge
                      color="light"
                      pill
                      className="text-dark badge-marketing"
                    >
                      {t("enterprise")}
                    </Badge>
                    <div className="pricing-price">
                      <sup>{t("usd")}</sup>
                      599
                      <span className="pricing-price-period">/{t("mo")}</span>
                    </div>
                  </div>
                  <List type="unstyled" className="fa-ul pricing-list">
                    <li className="pricing-list-item">
                      <span className="fa-li me-2">
                        <FaCheckCircle className="text-teal" />
                      </span>
                      <span className="text-dark">{t("unlimitedUsers")}</span>
                    </li>
                    <li className="pricing-list-item">
                      <span className="fa-li me-2">
                        <FaCheckCircle className="text-teal" />
                      </span>
                      <span className="text-dark">
                        {t("unlimitedProducts")}
                      </span>
                    </li>
                    <li className="pricing-list-item">
                      <span className="fa-li me-2">
                        <FaCheckCircle className="text-teal" />
                      </span>
                      <span className="text-dark">
                        {t("ticketAndLiveChatSupport")}
                      </span>
                    </li>
                    <li className="pricing-list-item">
                      <span className="fa-li me-2">
                        <FaCheckCircle className="text-teal" />
                      </span>
                      <span className="text-dark">{t("styleCustomizer")}</span>
                    </li>
                    <li className="pricing-list-item">
                      <span className="fa-li me-2">
                        <FaCheckCircle className="text-teal" />
                      </span>
                      <span className="text-dark">{t("customComponents")}</span>
                    </li>
                    <li className="pricing-list-item">
                      <span className="fa-li me-2">
                        <FaCheckCircle className="text-teal" />
                      </span>
                      <span className="text-dark">
                        {t("expandedUtilities")}
                      </span>
                    </li>
                    <li className="pricing-list-item">
                      <span className="fa-li me-2">
                        <FaCheckCircle className="text-teal" />
                      </span>
                      <span className="text-dark">
                        {t("thirdPartyIntegration")}
                      </span>
                    </li>
                    <li className="pricing-list-item">
                      <span className="fa-li me-2">
                        <FaCheckCircle className="text-teal" />
                      </span>
                      <span className="text-dark">{t("layoutOptions")}</span>
                    </li>
                  </List>
                </CardBody>
              </Card>
            </Col>
          </Row>
        </Container>
        <SvgBorder className="text-dark" />
      </section>
      <section className="bg-dark py-10">
        <Container className="px-5">
          <Row className="gx-5 my-10">
            <Col lg={6} className="mb-5">
              <div className="d-flex h-100">
                <div className="icon-stack flex-shrink-0 bg-teal text-white">
                  <FaQuestionCircle />
                </div>
                <div className="ms-4">
                  <h5 className="text-white">{t("whatIsCommunitySupport")}</h5>
                  <p className="text-white-50">
                    {t("communitySupportDescription")}
                  </p>
                </div>
              </div>
            </Col>
            <Col lg={6} className="mb-5">
              <div className="d-flex h-100">
                <div className="icon-stack flex-shrink-0 bg-teal text-white">
                  <FaQuestionCircle />
                </div>
                <div className="ms-4">
                  <h5 className="text-white">
                    {t("whatCanIDoWithPayedPlans")}
                  </h5>
                  <p className="text-white-50">
                    {t("whatCanIDoWithPayedPlansDescription")}
                  </p>
                </div>
              </div>
            </Col>
            <Col lg={6} className="mb-5 mb-lg-0">
              <div className="d-flex h-100">
                <div className="icon-stack flex-shrink-0 bg-teal text-white">
                  <FaQuestionCircle />
                </div>
                <div className="ms-4">
                  <h5 className="text-white">{t("doIWillGetUpdates")}</h5>
                  <p className="text-white-50">
                    {t("doIWillGetUpdatesDescription")}
                  </p>
                </div>
              </div>
            </Col>
            <Col lg={6}>
              <div className="d-flex h-100">
                <div className="icon-stack flex-shrink-0 bg-teal text-white">
                  <FaQuestionCircle />
                </div>
                <div className="ms-4">
                  <h5 className="text-white">{t("whatDoesItIntegrateWith")}</h5>
                  <p className="text-white-50">
                    {t("whatDoesItIntegrateWithDescription")}
                  </p>
                </div>
              </div>
            </Col>
          </Row>
          <Row className="gx-5 justify-content-center text-center">
            <Col lg={8}>
              <Badge
                color="light"
                pill
                className="bg-transparent-light badge-marketing mb-4"
              >
                {t("getStarted")}
              </Badge>
              <h2 className="text-white">{t("getStartedTitle")}</h2>
              <p className="lead text-white-50 mb-5">
                {t("getStartedDescription")}
              </p>
              <Button
                color="teal"
                className="fw-500"
                href="/auth/register"
                tag={Link}
              >
                {t("buyNow")}!
              </Button>
            </Col>
          </Row>
        </Container>
        <SvgBorder className="text-white" />
      </section>
    </>
  );
};
