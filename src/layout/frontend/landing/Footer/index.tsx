"use client";

import { useTranslations } from "next-intl";
import { useState } from "react";
import { Col, Container, Row } from "reactstrap";

import { APP_META_TITLE } from "@/environment";
import { Link, usePathname } from "@/navigation";

import { FooterMenu } from "./FooterMenu";

export const Footer = () => {
  const pathname = usePathname();
  const [color] = useState(
    pathname === "/" ? "bg-light footer-light" : "bg-dark footer-dark"
  );

  const t = useTranslations("translations");

  return (
    <div id="layoutDefault_footer">
      <footer className={`footer py-5 mt-auto ${color}`}>
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
              <Link href={"/privacy-policy"}>{t("privacyPolicy")}</Link>
              {" · "}
              <Link href={"/terms-and-conditions"}>
                {t("termsAndConditions")}
              </Link>
            </Col>
          </Row>
        </Container>
      </footer>
    </div>
  );
};
