"use client";

import Image from "next/image";
import { useTranslations } from "next-intl";
import { useEffect } from "react";
import { FiRefreshCw } from "react-icons/fi";
import { Col, Container, Row } from "reactstrap";

import { SvgBorder } from "@/components/frontend/common/SvgBorder";
import { APP_META_TITLE } from "@/environment";
import { Header } from "@/layout/frontend/landing/Header";
import { LandingLayout } from "@/layout/frontend/landing/LandingLayout";
import { Link } from "@/navigation";

export default function ErrorPage({
  error,
  reset,
}: {
  error: Error & { digest?: string };
  reset: () => void;
}) {
  const t = useTranslations("translations");

  const reload = (e: React.MouseEvent<HTMLAnchorElement>) => {
    e.preventDefault();
    reset();
  };

  useEffect(() => {
    console.error(error);
  }, [error]);

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
                  src="/assets/images/theme/landing/500-internal-server-error.svg"
                  width={562}
                  height={375}
                  alt={APP_META_TITLE}
                  className="img-fluid pb-4 text-purple"
                />
                <p className="lead">{t("internalServerErrorInfo")}</p>
                <Link className="text-arrow-icon" href="#!" onClick={reload}>
                  <FiRefreshCw width={24} height={24} className="ms-0 me-1" />
                  {t("reload")}
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
