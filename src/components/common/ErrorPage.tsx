"use client";

import Image from "next/image";
import { useTranslations } from "next-intl";
import { Col, Container } from "reactstrap";

import { Link, useRouter } from "@/navigation";
import { CommonErrorPageProps } from "@/types/common";

export const ErrorPage = ({
  title,
  description,
  titleClassName,
  BtnClassName,
}: CommonErrorPageProps) => {
  const t = useTranslations("translations");
  const router = useRouter();

  return (
    <div className="page-wrapper compact-wrapper" id="pageWrapper">
      <div className="error-wrapper">
        <Container>
          <Image
            width={100}
            height={100}
            className="img-100"
            src={`/assets/images/other-images/sad.png`}
            alt="Error"
          />
          <div className="error-heading">
            <h2 className={`headline ${titleClassName}`}>{title}</h2>
          </div>
          <Col md={8} className="offset-md-2">
            <p className="sub-content">{description}</p>
          </Col>
          <div>
            <Link
              className={`btn  ${BtnClassName} btn-lg text-uppercase`}
              href={"#"}
              onClick={() => router.back()}
            >
              {t("backToPreviousPage")}
            </Link>
          </div>
        </Container>
      </div>
    </div>
  );
};
