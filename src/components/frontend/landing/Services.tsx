"use client";

import { useTranslations } from "next-intl";
import { useState } from "react";
import { BsArrowRight } from "react-icons/bs";
import {
  Button,
  Col,
  Container,
  List,
  Modal,
  ModalBody,
  ModalFooter,
  ModalHeader,
  Row,
} from "reactstrap";

import { Link } from "@/navigation";

export const Services = () => {
  const [layouts, setLayouts] = useState(false);
  const [payments, setPayments] = useState(false);
  const [robustPlatform, setRobustPlatform] = useState(false);
  const [landingPages, setLandingPages] = useState(false);

  const toggleLayouts = (e: any) => {
    e?.preventDefault();
    setLayouts(!layouts);
  };

  const togglePayments = (e: any) => {
    e?.preventDefault();
    setPayments(!payments);
  };

  const toggleRobustPlatform = (e: any) => {
    e?.preventDefault();
    setRobustPlatform(!robustPlatform);
  };

  const toggleLandingPages = (e: any) => {
    e?.preventDefault();
    setLandingPages(!landingPages);
  };

  const t = useTranslations("translations");

  return (
    <>
      <section className="bg-light py-10">
        <Container className="px-5">
          <Row className="gx-5 align-items-center justify-content-center">
            <Col
              md={9}
              lg={6}
              className="order-1 order-lg-0 aos-init aos-animate"
              data-aos="fade-up"
            >
              <div className="content-skewed content-skewed-right">
                <img
                  className="content-skewed-item img-fluid shadow-lg rounded-3"
                  src="/assets/images/theme/landing/loja-virtual-1.svg"
                  alt=""
                />
              </div>
            </Col>
            <Col
              lg={6}
              className="order-0 order-lg-1 mb-5 mb-lg-0 aos-init aos-animate"
              data-aos="fade-up"
            >
              <div className="mb-5">
                <h2>{t("whatYouWillGet")}</h2>
                <p className="lead">{t("whatYouWillGetDescription")}</p>
              </div>
              <Row className="gx-5">
                <Col md={6} className="mb-4">
                  <h6>{t("landingPages")}</h6>
                  <p className="mb-2 small">{t("landingPagesDescription")}</p>
                  <Link
                    className="small text-arrow-icon"
                    href="#!"
                    onClick={toggleLandingPages}
                  >
                    {t("learnMore")}
                    <BsArrowRight />
                  </Link>
                </Col>
                <Col md={6} className="mb-4">
                  <h6>{t("paymentMethods")}</h6>
                  <p className="mb-2 small mb-0">
                    {t("paymentMethodsDescription")}
                  </p>
                  <Link
                    className="small text-arrow-icon"
                    href="#!"
                    onClick={togglePayments}
                  >
                    {t("learnMore")}
                    <BsArrowRight />
                  </Link>
                </Col>
              </Row>
              <Row className="gx-5">
                <Col md={6} className="mb-4">
                  <h6>{t("layouts")}</h6>
                  <p className="mb-2 small mb-0">{t("layoutsInfo")}</p>
                  <Link
                    className="small text-arrow-icon"
                    href="#!"
                    onClick={toggleLayouts}
                  >
                    {t("learnMore")}
                    <BsArrowRight />
                  </Link>
                </Col>
                <Col md={6} className="mb-4">
                  <h6>{t("robustPlatform")}</h6>
                  <p className="small mb-0">{t("robustPlatformDescription")}</p>
                  <Link
                    className="small text-arrow-icon"
                    href="#!"
                    onClick={toggleRobustPlatform}
                  >
                    {t("learnMore")}
                    <BsArrowRight />
                  </Link>
                </Col>
              </Row>
            </Col>
          </Row>
        </Container>
      </section>
      <hr className="m-0" />
      <Modal isOpen={layouts} toggle={toggleLayouts} size="lg">
        <ModalHeader toggle={toggleLayouts} className="text-uppercase">
          {t("layouts")}
        </ModalHeader>
        <ModalBody>
          <p>{t("speedUpYourWork")}</p>
          <p>
            <strong>{t("advantages")}: </strong>
          </p>
          <List type="unordered">
            <li>
              <strong>{t("customization")}: </strong>
              {t("customizationInfo")}
            </li>
            <li>
              <strong>{t("professionalQuality")}: </strong>
              {t("professionalQualityInfo")}
            </li>
            <li>
              <strong>{t("easeOfUse")}: </strong>
              {t("easeOfUseInfo")}
            </li>
            <li>
              <strong>{t("variety")}: </strong>
              {t("varietyInfo")}
            </li>
          </List>
          <p>{t("layoutsInfo2")}</p>
        </ModalBody>
        <ModalFooter>
          <Button color="secondary" onClick={toggleLayouts}>
            {t("close")}
          </Button>
        </ModalFooter>
      </Modal>
      <Modal isOpen={payments} toggle={togglePayments} size="lg">
        <ModalHeader toggle={togglePayments} className="text-uppercase">
          {t("paymentMethods")}
        </ModalHeader>
        <ModalBody>
          <p>{t("paymentsInfo")}</p>
          <List type="unordered">
            <li>
              <strong>{t("creditCard")}: </strong>
              {t("creditCardInfo")}
            </li>
            <li>
              <strong>{t("debitCard")}: </strong>
              {t("debitCardInfo")}
            </li>
            <li>
              <strong>{t("billet")}: </strong>
              {t("billetInfo")}
            </li>
            <li>
              <strong>{t("pix")}: </strong>
              {t("pixInfo")}
            </li>
          </List>
          <p>
            <strong>{t("paymentsInfo2")}</strong>
          </p>
          <p>{t("paymentsInfo3")}</p>{" "}
        </ModalBody>
        <ModalFooter>
          <Button color="secondary" onClick={togglePayments}>
            {t("close")}
          </Button>
        </ModalFooter>
      </Modal>
      <Modal isOpen={robustPlatform} toggle={toggleRobustPlatform} size="lg">
        <ModalHeader toggle={toggleRobustPlatform} className="text-uppercase">
          {t("robustPlatform")}
        </ModalHeader>
        <ModalBody>
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
        </ModalBody>
        <ModalFooter>
          <Button color="secondary" onClick={toggleRobustPlatform}>
            {t("close")}
          </Button>
        </ModalFooter>
      </Modal>
      <Modal isOpen={landingPages} toggle={toggleLandingPages} size="lg">
        <ModalHeader toggle={toggleLandingPages} className="text-uppercase">
          {t("landingPages")}
        </ModalHeader>
        <ModalBody>
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
        </ModalBody>
        <ModalFooter>
          <Button color="secondary" onClick={toggleLandingPages}>
            {t("close")}
          </Button>
        </ModalFooter>
      </Modal>
    </>
  );
};
