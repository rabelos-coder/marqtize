import { getTranslations } from "next-intl/server";
import { BsArrowRight, BsCart, BsCreditCard } from "react-icons/bs";
import { FaCheckCircle, FaCircle, FaQuestionCircle } from "react-icons/fa";
import { FiActivity, FiUser } from "react-icons/fi";
import { Badge, Card, CardBody, Col, Container, List, Row } from "reactstrap";

import { SvgBorder } from "@/components/frontend/common/SvgBorder";
import { Header } from "@/layout/frontend/landing/Header";
import { LandingLayout } from "@/layout/frontend/landing/LandingLayout";
import { Link } from "@/navigation";

import GoogleIcon from "../assets/svg/google.svg";
import InstagramIcon from "../assets/svg/instagram.svg";

export default async function HomePage({ params: { locale } }: any) {
  const t = await getTranslations({ locale });

  return (
    <LandingLayout>
      <Header>
        <Container className="px-5">
          <Row className="align-items-center gx-5">
            <Col
              lg={6}
              className="aos-init aos-animate text-white"
              data-aos="fade-up"
            >
              <h1 className="page-header-ui-title">{t("headerHome")}</h1>
              <p className="page-header-ui-text mb-5">
                {t.rich("whoWeAreDescription", {
                  b: (chunk) => <strong>{chunk}</strong>,
                })}
              </p>
            </Col>
            <Col
              lg={6}
              className="d-none d-lg-block aos-init aos-animate"
              data-aos="fade-up"
              data-aos-delay="100"
            >
              <img
                className="img-fluid"
                src="/assets/images/theme/landing/loja-virtual-2.svg"
                alt=""
              />
            </Col>
          </Row>
        </Container>
      </Header>
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
              <p className="mb-0">
                {t("improveCustomerExperienceDescription")}
              </p>
            </Col>
            <Col lg={4}>
              <div className="icon-stack icon-stack-xl bg-gradient-primary-to-secondary text-white mb-4">
                <BsCreditCard />
              </div>
              <h3>{t("incomeDiversification")}</h3>
              <p className="mb-0">{t("incomeDiversificationDescription")}</p>
            </Col>
          </Row>
        </Container>
        <SvgBorder className="text-light" />
      </section>
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
                    href="/about/landing-pages"
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
                    href="/about/payments"
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
                  <Link className="small text-arrow-icon" href="/about/layouts">
                    {t("learnMore")}
                    <BsArrowRight />
                  </Link>
                </Col>
                <Col md={6} className="mb-4">
                  <h6>{t("robustPlatform")}</h6>
                  <p className="small mb-0">{t("robustPlatformDescription")}</p>
                  <Link
                    className="small text-arrow-icon"
                    href="/about/robust-platform"
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
              <Link
                color="teal"
                className="btn btn-primary fw-500"
                href="/auth/register"
              >
                {t("buyNow")}!
              </Link>
            </Col>
          </Row>
        </Container>
        <SvgBorder className="text-white" />
      </section>
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
                  "Adipisci mollitia nemo magnam iure, temporibus molestiae
                  odit, sit harum dolores neque maiores quo eligendi nam
                  corrupti."
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
                      <h5 className="text-secondary">
                        {t("builtForCustomers")}
                      </h5>
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
              <Link
                color="primary"
                className="btn btn-secondary fw-500 me-3 my-2"
                href="#!"
              >
                {t("contactSales")}
              </Link>
              <Link
                color="white"
                className="btn btn-primary fw-500 my-2 shadow"
                href="/auth/register"
              >
                {t("createAccount")}
              </Link>
            </Col>
          </Row>
        </Container>
      </section>
      <hr className="m-0" />
    </LandingLayout>
  );
}
