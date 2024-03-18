import { getTranslations } from "next-intl/server";
import { FiArrowRight, FiLifeBuoy, FiTv, FiUser } from "react-icons/fi";
import { Col, Container, Row } from "reactstrap";

import { SvgBorder } from "@/components/frontend/common/SvgBorder";
import { EMAIL } from "@/environment";
import { Header } from "@/layout/frontend/landing/Header";
import { LandingLayout } from "@/layout/frontend/landing/LandingLayout";
import { Link } from "@/navigation";
import { concatTitle } from "@/utils/helpers";

export async function generateMetadata({ params: { locale } }: any) {
  const t = await getTranslations({ locale });
  const title = concatTitle(t("support"));

  return {
    title,
  };
}

export default async function SupportPage({ params: { locale } }: any) {
  const t = await getTranslations({ locale });

  return (
    <LandingLayout>
      <Header title={t("support")} description={t("supportDescription")} />
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
                  <h5>{t("sales")}</h5>
                  <p className="card-text">{t("salesSupportDescription")}</p>
                </div>
                <div className="card-footer">
                  <div className="text-primary fw-bold d-inline-flex align-items-center">
                    {t("contactName", { name: t("sales") })}
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
                  <h5>{t("support")}</h5>
                  <p className="card-text">{t("supportShortDescription")}</p>
                </div>
                <div className="card-footer">
                  <div className="text-secondary fw-bold d-inline-flex align-items-center">
                    {t("contactName", { name: t("support") })}
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
                  <h5>{t("media")}</h5>
                  <p className="card-text">{t("mediaSupportDescription")}</p>
                </div>
                <div className="card-footer">
                  <div className="text-teal fw-bold d-inline-flex align-items-center">
                    {t("contactName", { name: t("media") })}
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
              <h5>{t("joinUsOurCommunityForum")}</h5>
              <p className="fw-light mb-0">
                {t("joinUsOurCommunityForumDescription")}
                <Link href="/forum">{t("joinUsOurCommunityForumLink")}</Link>
              </p>
            </Col>
            <Col lg={5}>
              <h5>{t("generalSupport")}</h5>
              <p className="fw-light mb-0">
                {t("generalSupportDescription")}
                <Link href={`mailto:${EMAIL}`}>{EMAIL}</Link>
              </p>
            </Col>
          </Row>
        </Container>
        <SvgBorder className="text-dark" />
      </section>
    </LandingLayout>
  );
}
