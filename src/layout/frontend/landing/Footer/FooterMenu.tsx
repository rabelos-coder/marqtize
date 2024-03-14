import Image from "next/image";
import { useState } from "react";
import { FaFacebook, FaInstagram } from "react-icons/fa";
import { Col, Row } from "reactstrap";

import { APP_META_TITLE } from "@/environment";
import { usePathname } from "@/navigation";

export const FooterMenu = () => {
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
              width={120}
              height={120}
              alt={APP_META_TITLE}
              className="img-fluid me-2 mb-3"
            />
          </div>
          <div className="mb-3">Design made easy</div>
          <div className="icon-list-social mb-5">
            <a className="icon-list-social-link" href="#!">
              <FaInstagram />
            </a>
            <a className="icon-list-social-link" href="#!">
              <FaFacebook />
            </a>
          </div>
        </Col>
        <Col lg={9}>
          <Row className="gx-5">
            <Col lg={3} md={6} className="col-md-6 mb-5 mb-lg-0">
              <div className="text-uppercase-expanded text-xs mb-4">
                Product
              </div>
              <ul className="list-unstyled mb-0">
                <li className="mb-2">
                  <a href="#!">Landing</a>
                </li>
                <li className="mb-2">
                  <a href="#!">Pages</a>
                </li>
                <li className="mb-2">
                  <a href="#!">Sections</a>
                </li>
                <li className="mb-2">
                  <a href="#!">Documentation</a>
                </li>
                <li>
                  <a href="#!">Changelog</a>
                </li>
              </ul>
            </Col>
            <Col lg={3} md={6} className="mb-5 mb-lg-0">
              <div className="text-uppercase-expanded text-xs mb-4">
                Technical
              </div>
              <ul className="list-unstyled mb-0">
                <li className="mb-2">
                  <a href="#!">Documentation</a>
                </li>
                <li className="mb-2">
                  <a href="#!">Changelog</a>
                </li>
                <li className="mb-2">
                  <a href="#!">Theme Customizer</a>
                </li>
                <li>
                  <a href="#!">UI Kit</a>
                </li>
              </ul>
            </Col>
            <Col lg={3} md={6} className="mb-5 mb-md-0">
              <div className="text-uppercase-expanded text-xs mb-4">
                Includes
              </div>
              <ul className="list-unstyled mb-0">
                <li className="mb-2">
                  <a href="#!">Utilities</a>
                </li>
                <li className="mb-2">
                  <a href="#!">Components</a>
                </li>
                <li className="mb-2">
                  <a href="#!">Layouts</a>
                </li>
                <li className="mb-2">
                  <a href="#!">Code Samples</a>
                </li>
                <li className="mb-2">
                  <a href="#!">Products</a>
                </li>
                <li className="mb-2">
                  <a href="#!">Affiliates</a>
                </li>
                <li>
                  <a href="#!">Updates</a>
                </li>
              </ul>
            </Col>
            <Col lg={3} md={6}>
              <div className="text-uppercase-expanded text-xs mb-4">Legal</div>
              <ul className="list-unstyled mb-0">
                <li className="mb-2">
                  <a href="#!">Privacy Policy</a>
                </li>
                <li className="mb-2">
                  <a href="#!">Terms and Conditions</a>
                </li>
                <li>
                  <a href="#!">License</a>
                </li>
              </ul>
            </Col>
          </Row>
        </Col>
      </Row>
      <hr className="my-5"></hr>
    </>
  );
};
