"use client";

import Image from "next/image";
import { useTranslations } from "next-intl";
import { useState } from "react";
import { FaFacebook, FaInstagram } from "react-icons/fa";
import { Col, List, Row } from "reactstrap";

import { APP_META_TITLE, FACEBOOK_URL, INSTAGRAM_URL } from "@/environment";
import { Link, usePathname } from "@/navigation";

export const FooterMenu = () => {
  const t = useTranslations("translations");

  const pathname = usePathname();
  const [imgSrc] = useState(
    pathname === "/"
      ? "/assets/images/logo/marqtize_logo.png"
      : "/assets/images/logo/marqtize_logo_dark.png"
  );

  return (
    <>
      <Row className="gx-5">
        <Col lg={3}>
          <div className="footer-brand">
            <Image
              src={imgSrc}
              width={180}
              height={180}
              alt={APP_META_TITLE}
              className="img-fluid me-2 mb-3"
            />
          </div>
          <div className="mb-3">
            {t("madeWithLove", { company: APP_META_TITLE })}
          </div>
          <div className="icon-list-social mb-5">
            <Link
              className="icon-list-social-link"
              href={INSTAGRAM_URL}
              target="_blank"
            >
              <FaInstagram />
            </Link>
            <Link
              className="icon-list-social-link"
              href={FACEBOOK_URL}
              target="_blank"
            >
              <FaFacebook />
            </Link>
          </div>
        </Col>
        <Col lg={9}>
          <Row className="gx-5">
            <Col lg={3} md={6} className="col-md-6 mb-5 mb-lg-0">
              <div className="text-uppercase-expanded text-xs mb-4">
                Product
              </div>
              <List type="unstyled" className="mb-0">
                <li className="mb-2">
                  <Link href="#!">Landing</Link>
                </li>
                <li className="mb-2">
                  <Link href="#!">Pages</Link>
                </li>
                <li className="mb-2">
                  <Link href="#!">Sections</Link>
                </li>
                <li className="mb-2">
                  <Link href="#!">Documentation</Link>
                </li>
                <li>
                  <Link href="#!">Changelog</Link>
                </li>
              </List>
            </Col>
            <Col lg={3} md={6} className="mb-5 mb-lg-0">
              <div className="text-uppercase-expanded text-xs mb-4">
                Technical
              </div>
              <List type="unstyled" className="mb-0">
                <li className="mb-2">
                  <Link href="#!">Documentation</Link>
                </li>
                <li className="mb-2">
                  <Link href="#!">Changelog</Link>
                </li>
                <li className="mb-2">
                  <Link href="#!">Theme Customizer</Link>
                </li>
                <li>
                  <Link href="#!">UI Kit</Link>
                </li>
              </List>
            </Col>
            <Col lg={3} md={6} className="mb-5 mb-md-0">
              <div className="text-uppercase-expanded text-xs mb-4">
                Includes
              </div>
              <List type="unstyled" className="mb-0">
                <li className="mb-2">
                  <Link href="#!">Utilities</Link>
                </li>
                <li className="mb-2">
                  <Link href="#!">Components</Link>
                </li>
                <li className="mb-2">
                  <Link href="#!">Layouts</Link>
                </li>
                <li className="mb-2">
                  <Link href="#!">Code Samples</Link>
                </li>
                <li className="mb-2">
                  <Link href="#!">Products</Link>
                </li>
                <li className="mb-2">
                  <Link href="#!">Affiliates</Link>
                </li>
                <li>
                  <Link href="#!">Updates</Link>
                </li>
              </List>
            </Col>
            <Col lg={3} md={6}>
              <div className="text-uppercase-expanded text-xs mb-4">
                {t("legal")}
              </div>
              <List type="unstyled" className="mb-0">
                <li className="mb-2">
                  <Link href="/privacy-policy">{t("privacyPolicy")}</Link>
                </li>
                <li className="mb-2">
                  <Link href="/terms-and-conditions">
                    {t("termsAndConditions")}
                  </Link>
                </li>
                <li>
                  <Link href="/license">{t("license")}</Link>
                </li>
              </List>
            </Col>
          </Row>
        </Col>
      </Row>
      <hr className="my-5"></hr>
    </>
  );
};
