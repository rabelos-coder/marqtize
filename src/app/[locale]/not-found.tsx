"use client";

import Image from "next/image";
import { useTranslations } from "next-intl";
import { FiArrowLeft } from "react-icons/fi";
import { Col, Container, Row } from "reactstrap";

import { SvgBorder } from "@/components/frontend/common/SvgBorder";
import { APP_META_TITLE } from "@/environment";
import { Header } from "@/layout/frontend/landing/Header";
import { LandingLayout } from "@/layout/frontend/landing/LandingLayout";
import { Link, useRouter } from "@/navigation";

export default function NotFound() {
  const t = useTranslations();

  const router = useRouter();

  const handleBack = (e: React.MouseEvent<HTMLAnchorElement>) => {
    e.preventDefault();
    router.back();
  };

  return (
    <LandingLayout>
      <Header
        className="not-found"
        style={{ paddingTop: "6rem", paddingBottom: "6rem" }}
      />
      <section className="bg-white py-10">
        <Container className="px-5">
          <Row className="gx-5 justify-content-center">
            <Col xl={6}>
              <div className="text-center mt-4">
                <Image
                  src="/assets/images/theme/landing/404-error.svg"
                  width={462}
                  height={287}
                  alt={APP_META_TITLE}
                  className="img-fluid pb-4 text-purple"
                />
                <p className="lead">{t("notFoundInfo")}</p>
                <Link
                  className="text-arrow-icon"
                  href="#!"
                  onClick={handleBack}
                >
                  <FiArrowLeft width={24} height={24} className="ms-0 me-1" />
                  {t("back")}
                </Link>
              </div>
            </Col>
          </Row>
        </Container>
        <SvgBorder className="text-dark" />
      </section>
    </LandingLayout>
  );
}
