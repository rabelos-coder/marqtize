import { getTranslations } from "next-intl/server";
import { FiArrowRight, FiLifeBuoy, FiTv, FiUser } from "react-icons/fi";
import { Col, Container, Row } from "reactstrap";

import { SvgBorder } from "@/components/frontend/common/SvgBorder";
import { Header } from "@/layout/frontend/landing/Header";
import { LandingLayout } from "@/layout/frontend/landing/LandingLayout";
import { concatTitle } from "@/utils/helpers";

export async function generateMetadata({ params: { locale } }: any) {
  const t = await getTranslations({ locale, namespace: "translations" });
  const title = concatTitle(t("support"));

  return {
    title,
  };
}

export default function SupportPage() {
  return (
    <LandingLayout>
      <Header
        title="Support"
        description="Create beautiful pages with easy to edit content"
      />
      <section className="bg-white py-10">
        <Container className="px-5">
          <Row className="gx-5 mb-5">
            <Col lg={4} className="mb-5">
              <a
                className="card card-link border-bottom-0 border-start-0 border-end-0 border-top-lg border-primary lift text-center o-visible h-100"
                href="#!"
              >
                <div className="card-body">
                  <div className="icon-stack icon-stack-xl bg-primary-soft text-primary mb-4 mt-n5 z-1 shadow">
                    <FiUser width={24} height={24} />
                  </div>
                  <h5>Sales</h5>
                  <p className="card-text">
                    Ready to open an account? Have questions about purchasing a
                    product?
                  </p>
                </div>
                <div className="card-footer">
                  <div className="text-primary fw-bold d-inline-flex align-items-center">
                    Contact Sales
                    <FiArrowRight
                      className="text-xs ms-1"
                      width={24}
                      height={24}
                    />
                  </div>
                </div>
              </a>
            </Col>
            <Col lg={4} className="mb-5">
              <a
                className="card card-link border-bottom-0 border-start-0 border-end-0 border-top-lg border-secondary lift text-center o-visible h-100"
                href="#!"
              >
                <div className="card-body">
                  <div className="icon-stack icon-stack-xl bg-secondary-soft text-secondary mb-4 mt-n5 z-1 shadow">
                    <FiLifeBuoy width={24} height={24} />
                  </div>
                  <h5>Support</h5>
                  <p className="card-text">
                    Need help with a product that you just purchased? Need help
                    with your account?
                  </p>
                </div>
                <div className="card-footer">
                  <div className="text-secondary fw-bold d-inline-flex align-items-center">
                    Contact Support
                    <FiArrowRight
                      className="text-xs ms-1"
                      width={24}
                      height={24}
                    />
                  </div>
                </div>
              </a>
            </Col>
            <Col lg={4} className="mb-5">
              <a
                className="card card-link border-bottom-0 border-start-0 border-end-0 border-top-lg border-teal lift text-center o-visible h-100"
                href="#!"
              >
                <div className="card-body">
                  <div className="icon-stack icon-stack-xl bg-teal-soft text-teal mb-4 mt-n5 z-1 shadow">
                    <FiTv width={24} height={24} />
                  </div>
                  <h5>Media</h5>
                  <p className="card-text">
                    Looking to contact our media team for a press release or
                    related story?
                  </p>
                </div>
                <div className="card-footer">
                  <div className="text-teal fw-bold d-inline-flex align-items-center">
                    Contact Media
                    <FiArrowRight
                      className="text-xs ms-1"
                      width={24}
                      height={24}
                    />
                  </div>
                </div>
              </a>
            </Col>
          </Row>
          <Row className="gx-5 justify-content-center text-center">
            <Col lg={5} className="mb-5 mb-lg-0">
              <h5>Join us on Discord!</h5>
              <p className="fw-light mb-0">
                Join the discussion on Discord. Our community can help answer
                questions!
              </p>
            </Col>
            <Col lg={5}>
              <h5>General Support</h5>
              <p className="fw-light mb-0">
                For any other support questions, please send us an email at{" "}
                <a href="#!">support@example.com</a>
              </p>
            </Col>
          </Row>
          <hr className="my-10" />
          <Row className="gx-5 justify-content-center">
            <Col lg={8} className="text-center">
              <h2>Can't find the answer you need?</h2>
              <p className="lead mb-5">
                Contact us and we'll get back to you as soon as possible with a
                solution to whatever issues you're having with SB UI Kit Pro.
              </p>
            </Col>
          </Row>
          <Row className="gx-5 align-items-center mb-10">
            <Col lg={4} className="text-center mb-5 mb-lg-0">
              <div className="section-preheading">Message Us</div>
              <a href="#!">Start a chat!</a>
            </Col>
            <Col lg={4} className="text-center mb-5 mb-lg-0">
              <div className="section-preheading">Call Anytime</div>
              <a href="#!">(555) 565-1846</a>
            </Col>
            <Col lg={4} className="text-center">
              <div className="section-preheading">Email Us</div>
              <a href="#!">support@startbootstrap.com</a>
            </Col>
          </Row>
          <form>
            <Row className="gx-5 mb-4">
              <Col md={6}>
                <label className="text-dark mb-2" htmlFor="inputName">
                  Full name
                </label>
                <input
                  className="form-control py-4"
                  id="inputName"
                  type="text"
                  placeholder="Full name"
                />
              </Col>
              <Col md={6}>
                <label className="text-dark mb-2" htmlFor="inputEmail">
                  Email
                </label>
                <input
                  className="form-control py-4"
                  id="inputEmail"
                  type="email"
                  placeholder="name@example.com"
                />
              </Col>
            </Row>
            <div className="mb-4">
              <label className="text-dark mb-2" htmlFor="inputMessage">
                Message
              </label>
              <textarea
                className="form-control py-3"
                id="inputMessage"
                placeholder="Enter your message..."
                rows={4}
              ></textarea>
            </div>
            <div className="text-center">
              <button className="btn btn-primary mt-4" type="submit">
                Submit Request
              </button>
            </div>
          </form>
        </Container>
        <SvgBorder className="text-dark" />
      </section>
    </LandingLayout>
  );
}
