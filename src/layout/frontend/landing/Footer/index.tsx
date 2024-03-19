"use client";

import { useTranslations } from "next-intl";
import { Col, Container, Row } from "reactstrap";

import { APP_META_TITLE } from "@/environment";
import { Link } from "@/navigation";

import { FooterMenu } from "./FooterMenu";

export const Footer = () => {
  const t = useTranslations();

  return (
    <div id="layoutDefault_footer">
      <footer className={`footer py-5 mt-auto bg-dark footer-dark`}>
        <Container className="px-5">
          <FooterMenu />
          <Row className="gx-5 align-items-center">
            <Col md={6}>
              {t("copyright", {
                year: new Date().getFullYear(),
                company: APP_META_TITLE,
              })}
            </Col>
            <Col md={6} className="text-md-end">
              <Link href={"/privacy-policy"}>{t("privacyPolicy.title")}</Link>
              {" · "}
              <Link href={"/terms-and-conditions"}>
                {t("termsAndConditions.title")}
              </Link>
            </Col>
          </Row>
        </Container>
      </footer>
    </div>
  );
};
