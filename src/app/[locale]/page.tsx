import { getTranslations } from 'next-intl/server'
import { BsArrowRight } from '@react-icons/all-files/bs/BsArrowRight'
import { FaCartPlus } from '@react-icons/all-files/fa/FaCartPlus'
import { BsCreditCard } from '@react-icons/all-files/bs/BsCreditCard'
import { FaCheckCircle } from '@react-icons/all-files/fa/FaCheckCircle'
import { FaCircle } from '@react-icons/all-files/fa/FaCircle'
import { FaQuestionCircle } from '@react-icons/all-files/fa/FaQuestionCircle'
import { FiUser } from '@react-icons/all-files/fi/FiUser'
import { Badge, Card, CardBody, Col, Container, List, Row } from 'reactstrap'

import SvgBorder from '@/components/frontend/common/SvgBorder'
import Header from '@/layouts/frontend/landing/Header'
import LandingLayout from '@/layouts/frontend/landing/LandingLayout'
import { Link } from '@/navigation'

export default async function HomePage({ params: { locale } }: any) {
  const t = await getTranslations({ locale })

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
              <h1 className="page-header-ui-title">{t('headerHome')}</h1>
              <p className="page-header-ui-text mb-5">
                {t.rich('whoWeAreDescription', {
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
                src="/assets/images/themes/landing/loja-virtual-2.svg"
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
                <FaCartPlus />
              </div>
              <h3 id="builtFor">{t('increaseYourSales')}</h3>
              <p className="mb-0">{t('increaseYourSalesDescription')}</p>
            </Col>
            <Col lg={4} className="mb-5 mb-lg-0">
              <div className="icon-stack icon-stack-xl bg-gradient-primary-to-secondary text-white mb-4">
                <FiUser />
              </div>
              <h3>{t('improveAccountExperience')}</h3>
              <p className="mb-0">{t('improveAccountExperienceDescription')}</p>
            </Col>
            <Col lg={4}>
              <div className="icon-stack icon-stack-xl bg-gradient-primary-to-secondary text-white mb-4">
                <BsCreditCard />
              </div>
              <h3>{t('incomeDiversification')}</h3>
              <p className="mb-0">{t('incomeDiversificationDescription')}</p>
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
                  src="/assets/images/themes/landing/loja-virtual-1.svg"
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
                <h2>{t('whatYouWillGet')}</h2>
                <p className="lead">{t('whatYouWillGetDescription')}</p>
              </div>
              <Row className="gx-5">
                <Col md={6} className="mb-4">
                  <h6>{t('landingPages')}</h6>
                  <p className="mb-2 small">{t('landingPagesDescription')}</p>
                  <Link
                    className="small text-arrow-icon"
                    href="/about/landing-pages"
                  >
                    {t('learnMore')}
                    <BsArrowRight />
                  </Link>
                </Col>
                <Col md={6} className="mb-4">
                  <h6>{t('paymentMethods')}</h6>
                  <p className="mb-2 small mb-0">
                    {t('paymentMethodsDescription')}
                  </p>
                  <Link
                    className="small text-arrow-icon"
                    href="/about/payments"
                  >
                    {t('learnMore')}
                    <BsArrowRight />
                  </Link>
                </Col>
              </Row>
              <Row className="gx-5">
                <Col md={6} className="mb-4">
                  <h6>{t('layouts')}</h6>
                  <p className="mb-2 small mb-0">{t('layoutsInfo')}</p>
                  <Link className="small text-arrow-icon" href="/about/layouts">
                    {t('learnMore')}
                    <BsArrowRight />
                  </Link>
                </Col>
                <Col md={6} className="mb-4">
                  <h6>{t('robustPlatform')}</h6>
                  <p className="small mb-0">{t('robustPlatformDescription')}</p>
                  <Link
                    className="small text-arrow-icon"
                    href="/about/robust-platform"
                  >
                    {t('learnMore')}
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
            <h2>{t('pricingTable')}</h2>
            <p className="lead">{t('pricingTableDescription')}</p>
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
                      {t('free')}
                    </div>
                    <div className="pricing-price">
                      <sup>{t('usd')}</sup>0
                      <span className="pricing-price-period">/{t('mo')}</span>
                    </div>
                  </div>
                  <List type="unstyled" className="fa-ul pricing-list">
                    <li className="pricing-list-item">
                      <span className="fa-li me-2">
                        <FaCheckCircle className="text-teal" />
                      </span>
                      <span className="text-dark">1 {t('user')}</span>
                    </li>
                    <li className="pricing-list-item">
                      <span className="fa-li me-2">
                        <FaCheckCircle className="text-teal" />
                      </span>
                      <span className="text-dark">
                        {t('upToCountProducts', { count: 10 })}
                      </span>
                    </li>
                    <li className="pricing-list-item">
                      <span className="fa-li me-2">
                        <FaCheckCircle className="text-teal" />
                      </span>
                      <span className="text-dark">{t('communitySupport')}</span>
                    </li>
                    <li className="pricing-list-item">
                      <span className="fa-li me-2">
                        <FaCircle className="text-gray-200" />
                      </span>
                      {t('styleCustomizer')}
                    </li>
                    <li className="pricing-list-item">
                      <span className="fa-li me-2">
                        <FaCircle className="text-gray-200" />
                      </span>
                      {t('customComponents')}
                    </li>
                    <li className="pricing-list-item">
                      <span className="fa-li me-2">
                        <FaCircle className="text-gray-200" />
                      </span>
                      {t('expandedUtilities')}
                    </li>
                    <li className="pricing-list-item">
                      <span className="fa-li me-2">
                        <FaCircle className="text-gray-200" />
                      </span>
                      {t('thirdPartyIntegration')}
                    </li>
                    <li className="pricing-list-item">
                      <span className="fa-li me-2">
                        <FaCircle className="text-gray-200" />
                      </span>
                      {t('layoutOptions')}
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
                      {t('standard')}
                    </div>
                    <div className="pricing-price">
                      <sup>{t('usd')}</sup>
                      149
                      <span className="pricing-price-period">/{t('mo')}</span>
                    </div>
                  </div>
                  <List type="unstyled" className="fa-ul pricing-list">
                    <li className="pricing-list-item">
                      <span className="fa-li me-2">
                        <FaCheckCircle className="text-teal" />
                      </span>
                      <span className="text-dark">
                        {t('upToCountUsers', { count: 10 })}
                      </span>
                    </li>
                    <li className="pricing-list-item">
                      <span className="fa-li me-2">
                        <FaCheckCircle className="text-teal" />
                      </span>
                      <span className="text-dark">
                        {t('upToCountProducts', { count: 100 })}
                      </span>
                    </li>
                    <li className="pricing-list-item">
                      <span className="fa-li me-2">
                        <FaCheckCircle className="text-teal" />
                      </span>
                      <span className="text-dark">
                        {t('ticketAndLiveChatSupport')}
                      </span>
                    </li>
                    <li className="pricing-list-item">
                      <span className="fa-li me-2">
                        <FaCheckCircle className="text-teal" />
                      </span>
                      <span className="text-dark">{t('styleCustomizer')}</span>
                    </li>
                    <li className="pricing-list-item">
                      <span className="fa-li me-2">
                        <FaCheckCircle className="text-teal" />
                      </span>
                      <span className="text-dark">{t('customComponents')}</span>
                    </li>
                    <li className="pricing-list-item">
                      <span className="fa-li me-2">
                        <FaCheckCircle className="text-teal" />
                      </span>
                      <span className="text-dark">
                        {t('expandedUtilities')}
                      </span>
                    </li>
                    <li className="pricing-list-item">
                      <span className="fa-li me-2">
                        <FaCheckCircle className="text-teal" />
                      </span>
                      <span className="text-dark">
                        {t('thirdPartyIntegration')}
                      </span>
                    </li>
                    <li className="pricing-list-item">
                      <span className="fa-li me-2">
                        <FaCheckCircle className="text-teal" />
                      </span>
                      <span className="text-dark">{t('layoutOptions')}</span>
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
                      {t('enterprise')}
                    </Badge>
                    <div className="pricing-price">
                      <sup>{t('usd')}</sup>
                      599
                      <span className="pricing-price-period">/{t('mo')}</span>
                    </div>
                  </div>
                  <List type="unstyled" className="fa-ul pricing-list">
                    <li className="pricing-list-item">
                      <span className="fa-li me-2">
                        <FaCheckCircle className="text-teal" />
                      </span>
                      <span className="text-dark">{t('unlimitedUsers')}</span>
                    </li>
                    <li className="pricing-list-item">
                      <span className="fa-li me-2">
                        <FaCheckCircle className="text-teal" />
                      </span>
                      <span className="text-dark">
                        {t('unlimitedProducts')}
                      </span>
                    </li>
                    <li className="pricing-list-item">
                      <span className="fa-li me-2">
                        <FaCheckCircle className="text-teal" />
                      </span>
                      <span className="text-dark">
                        {t('ticketAndLiveChatSupport')}
                      </span>
                    </li>
                    <li className="pricing-list-item">
                      <span className="fa-li me-2">
                        <FaCheckCircle className="text-teal" />
                      </span>
                      <span className="text-dark">{t('styleCustomizer')}</span>
                    </li>
                    <li className="pricing-list-item">
                      <span className="fa-li me-2">
                        <FaCheckCircle className="text-teal" />
                      </span>
                      <span className="text-dark">{t('customComponents')}</span>
                    </li>
                    <li className="pricing-list-item">
                      <span className="fa-li me-2">
                        <FaCheckCircle className="text-teal" />
                      </span>
                      <span className="text-dark">
                        {t('expandedUtilities')}
                      </span>
                    </li>
                    <li className="pricing-list-item">
                      <span className="fa-li me-2">
                        <FaCheckCircle className="text-teal" />
                      </span>
                      <span className="text-dark">
                        {t('thirdPartyIntegration')}
                      </span>
                    </li>
                    <li className="pricing-list-item">
                      <span className="fa-li me-2">
                        <FaCheckCircle className="text-teal" />
                      </span>
                      <span className="text-dark">{t('layoutOptions')}</span>
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
                  <h5 className="text-white">{t('whatIsCommunitySupport')}</h5>
                  <p className="text-white-50">
                    {t('communitySupportDescription')}
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
                    {t('whatCanIDoWithPayedPlans')}
                  </h5>
                  <p className="text-white-50">
                    {t('whatCanIDoWithPayedPlansDescription')}
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
                  <h5 className="text-white">{t('doIWillGetUpdates')}</h5>
                  <p className="text-white-50">
                    {t('doIWillGetUpdatesDescription')}
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
                  <h5 className="text-white">{t('whatDoesItIntegrateWith')}</h5>
                  <p className="text-white-50">
                    {t('whatDoesItIntegrateWithDescription')}
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
                {t('getStarted')}
              </Badge>
              <h2 className="text-white">{t('getStartedTitle')}</h2>
              <p className="lead text-white-50 mb-5">
                {t('getStartedDescription')}
              </p>
              <Link
                color="teal"
                className="btn btn-primary fw-500"
                href="/auth/register"
              >
                {t('buyNow')}!
              </Link>
            </Col>
          </Row>
        </Container>
        <SvgBorder className="text-dark" />
      </section>
      <hr className="m-0" />
    </LandingLayout>
  )
}
